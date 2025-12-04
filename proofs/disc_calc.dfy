lemma {:induction false} fund_theorem_of_disc_calc(s: InfIntSeq, i: nat, j: nat)
    requires i <= j
    ensures sum_from_i_to_j(discrete_derivative(s), i, j) == 
            get_elem_at_index(s, j) - get_elem_at_index(s, i)
{
}
