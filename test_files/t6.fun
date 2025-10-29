x = 10

fun a() {
   write x
   z = x + 1
   write z
   fun b(i) {
       write i
       write z+i
   }
}


r = a()

x = 100

write(r(2))


