for i 0 4 {
    try {
        if (i == 1)
            (10 / 0)
        else
            write i
    } catch Arithmetic {
        write 100
        continue
    }
}

write 200
