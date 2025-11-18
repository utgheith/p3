let x = 5

fun makeAdder(delta) {
    fun inner(n) {
        x + delta + n
    }
    inner
}

let outer = 0

while (outer < 2) {
    let x = outer * 100
    write x
    let add = call makeAdder(outer)
    write (call add(1))
    let outer = outer + 1
}

let addGlobal = call makeAdder(10)
write (call addGlobal(0))

call addGlobal(5)
