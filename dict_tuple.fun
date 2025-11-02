let d = #[]
let d[0] = [1, 2]
write (d[0][0])
write (d[0][1])

let d[0][1] = 42
write (d[0][1])

let tup = [d[0][0], d[0][1]]
write tup

let d[1] = #[]
let d[1][0] = (tup[0] + tup[1])
write (d[1][0])

d[0][1]
