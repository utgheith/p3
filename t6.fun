let x = 10

fun a() {
   write x
   let z = x + 1
   write z
   fun b(i) {
       write i
       write z+i
   }
}


let r = call a()

let x = 100

write(call r(2))


