/*
    This testcase requires students to prove of the "Fundemental Theorem of Discrete Calculus".
    Infinite sequences of integers are represented using a codatatype 'InfIntSeq'.
*/

/* ========= InfIntSeq Functions ========= */

// InfIntSeq represents a possibly infinite sequence of integers
// If the sequence is finite, it is interpreted as being padded with 0s
codatatype InfIntSeq = End | InfIntSeqCons(head: int, tail: InfIntSeq)

function tail_after_n_elems(s: InfIntSeq, n: nat): InfIntSeq {
    if n == 0 then s
    else match s
        case End => End
        case InfIntSeqCons(h, t) => tail_after_n_elems(t, n - 1)
}

function get_elem_at_index(s: InfIntSeq, n: nat): int {
    match s
    case End => 0
    case InfIntSeqCons(h, t) =>
        if n == 0 then h
        else get_elem_at_index(t, n - 1)
}

/* ========= Discrete Derivative and Sum Functions ========= */

/*
    Let a_i be a sequence of ints (0-indexed).

    da_i = a_(i+1) - a_i (discrete derivative of a_i)
*/
function discrete_derivative(s: InfIntSeq): InfIntSeq
{
    match s
    case End => End
    case InfIntSeqCons(h1, t1) =>
        match t1
        case End => InfIntSeqCons(0 - h1, End) // Since we interpret finite InfIntSeq as being padded with 0s
        case InfIntSeqCons(h2, t2) =>
            InfIntSeqCons(h2 - h1, discrete_derivative(t1))
}

function sum_of_first_n(s: InfIntSeq, n: nat): int {
    if n == 0 then 0
    else match s
        case End => 0
        case InfIntSeqCons(h, t) => h + sum_of_first_n(t, n - 1)
}

function sum_from_i_to_j(s: InfIntSeq, i: nat, j: nat): int {
    if i >= j then 0
    else sum_of_first_n(tail_after_n_elems(s, i), j - i)
}

/* ========= Test Cases ========= */

/*
    The fundemental theorem of discrete calculus states that:
    Sum from k=i to j of (da_k) = a_j - a_i

    Aka sum of the individual changes gives the overall change
*/
method tc_fund_theorem_of_disc_calc(s: InfIntSeq, i: nat, j: nat) 
    requires i <= j
{
    assert sum_from_i_to_j(discrete_derivative(s), i, j) == 
           get_elem_at_index(s, j) - get_elem_at_index(s, i) by {
        fund_theorem_of_disc_calc(s, i, j);
    }
}

/*
    Nothing to do with the test case, but a cool thing:

    This theorem allows us to compute the sums of sequences in a manner similar to how 
    we evaluate integerals, by finding a "discrete antiderivative" of the sequence. 

    Here is a recursive algorithm you can use to figure out the discrete antiderivative of i^n:

        First guess i^(n+1). Using the binomial theorem, you will get an expression E involving powers of i from 0 to n.

        Subtract of the discrete antiderivatives of all the lower power terms of i in E from i^(n+1).

        Then divide by the coefficient of i^n in E, and you are done.

    -----

    This gives a mechanical process to discover formulas for the sums of powers of i
*/