//Provides: caml_stacks
var caml_stacks = []

//Provides: caml_alloc_stack
//Requires: caml_stacks
function caml_alloc_stack(hval, hexn, heff) {
    caml_stacks.push([hval, hexn, heff]);
    return 0;
}