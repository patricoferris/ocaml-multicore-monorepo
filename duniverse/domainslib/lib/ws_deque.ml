(*
 * Copyright (c) 2015, Théo Laurent <theo.laurent@ens.fr>
 * Copyright (c) 2015, KC Sivaramakrishnan <sk826@cl.cam.ac.uk>
 * Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
 * Copyright (c) 2021, Tom Kelly <ctk21@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(* Work Stealing Queue
 *
 * See:
 *   Dynamic circular work-stealing deque
 *   https://dl.acm.org/doi/10.1145/1073970.1073974
 *  &
 *   Correct and efficient work-stealing for weak memory models
 *   https://dl.acm.org/doi/abs/10.1145/2442516.2442524
 *)

module type S = sig
  type 'a t
  val create : unit -> 'a t
  val is_empty : 'a t -> bool
  val size : 'a t -> int
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a
  val steal : 'a t -> 'a
end

module CArray = struct

  type 'a t = {
    arr  : 'a array;
    mask : int
    }

  let rec log2 n =
    if n <= 1 then 0 else 1 + (log2 (n asr 1))

  let create n v =
    let sz = Int.shift_left 1 (log2 n) in
    assert ((sz >= n) && (sz > 0));
    assert (Int.logand sz (sz-1) == 0);
    {
      arr  = Array.make sz v;
      mask = sz - 1
    }

  let size t = Array.length t.arr [@@inline]

  let get t i =
    Array.unsafe_get t.arr (Int.logand i t.mask) [@@inline]

  let put t i v =
    Array.unsafe_set t.arr (Int.logand i t.mask) v [@@inline]

  let grow t top bottom =
    let s = size t in
    let ns = 2 * s in
    let out = create ns (Obj.magic ()) in
    for i = top to bottom do
      put out i (get t i)
    done;
    out

  let shrink t top bottom =
    let s = size t in
    let ns = s / 2 in
    let out = create ns (Obj.magic ()) in
    for i = top to bottom do
      put out i (get t i)
    done;
    out

end

module M : S = struct
  let min_size = 32
  let shrink_const = 3

  type 'a t = {
    top : int Atomic.t;
    bottom : int Atomic.t;
    tab : 'a ref CArray.t Atomic.t;
    mutable next_shrink : int;
  }

  let create () = {
    top = Atomic.make 1;
    bottom = Atomic.make 1;
    tab = Atomic.make (CArray.create min_size (Obj.magic ()));
    next_shrink = 0
  }

  let set_next_shrink q =
    let sz = CArray.size (Atomic.get q.tab) in
    if sz <= min_size then
      q.next_shrink <- 0
    else
      q.next_shrink <- sz / shrink_const

  let grow q t b =
    Atomic.set q.tab (CArray.grow (Atomic.get q.tab) t b);
    set_next_shrink q

  let is_empty q =
    let b = Atomic.get q.bottom in
    let t = Atomic.get q.top in
    b - t <= 0

  let size q =
    let b = Atomic.get q.bottom in
    let t = Atomic.get q.top in
    b - t

  let push q v =
    let v' = ref v in
    let b = Atomic.get q.bottom in
    let t = Atomic.get q.top in
    let a = Atomic.get q.tab in
    let size = b - t in
    let a =
      if size = CArray.size a then
        (grow q t b; Atomic.get q.tab)
      else
        a
    in
    CArray.put a b v';
    Atomic.set q.bottom (b + 1)

  let release ptr =
    let res = !ptr in
    (* we know this ptr will never be dereferenced, but want to
      break the reference to ensure that the contents of the
      deque array get garbage collected *)
    ptr := Obj.magic ();
    res
  [@@inline]

  let pop q =
    if size q = 0 then raise Exit
    else begin
      let b = (Atomic.get q.bottom) - 1 in
      Atomic.set q.bottom b;
      let t = Atomic.get q.top in
      let a = Atomic.get q.tab in
      let size = b - t in
      if size < 0 then begin
        (* empty queue *)
        Atomic.set q.bottom (b + 1);
        raise Exit
      end else
        let out = CArray.get a b in
        if b = t then begin
          (* single last element *)
          if (Atomic.compare_and_set q.top t (t + 1)) then
            (Atomic.set q.bottom (b + 1); release out)
          else
            (Atomic.set q.bottom (b + 1); raise Exit)
        end else begin
          (* non-empty queue *)
          if q.next_shrink > size then begin
            Atomic.set q.tab (CArray.shrink a t b);
            set_next_shrink q
          end;
          release out
        end
    end

  let rec steal q =
    let t = Atomic.get q.top in
    let b = Atomic.get q.bottom in
    let size = b - t in
    if size <= 0 then
      raise Exit
    else
      let a = Atomic.get q.tab in
      let out = CArray.get a t in
      if Atomic.compare_and_set q.top t (t + 1) then
        release out
      else begin
        Domain.cpu_relax ();
        steal q
      end

end
