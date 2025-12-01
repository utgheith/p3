// Program proofs, p. 96

function {:induction false} More(x : int) : int {
    if (x <= 0) then 1 else More(x-2) + 3
}



method MoreTest(x: int) {

    assert x < More(x) by {
        proof(x);
    }

   

    
}

