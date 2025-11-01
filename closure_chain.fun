fun outer(base) {
    fun mid(offset) {
        fun inner(n) {
            base + offset + n
        }
        inner
    }
    mid
}

let mid = call outer(5)
let inner = call mid(2)
write (call inner(3))

let mid10 = call outer(10)
let direct = call mid10(1)
write (call direct(4))
write (call direct(0))

call inner(0)
