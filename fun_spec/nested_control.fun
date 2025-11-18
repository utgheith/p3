let outer = 0

while (outer < 2) {
    for i 0 5 {
        if (i == 3)
            break
        else
            if (i == 1)
                continue
            else
                write ((outer * 10) + i)
    }
    let outer = outer + 1
}

write 42
