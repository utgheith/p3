let d = #[]
let d[5] = "five"
let d[10] = "ten"
write (d[5])
write (d[10])

let d[5] = "updated"
write (d[5])

let d2 = #[]
let d2[1] = #[]
let d2[1][2] = "nested"
write (d2[1][2])
