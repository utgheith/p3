// Proofs for the one time pad
// 
// We prove xor, because it's the most annoying
// You might not need all of these lemmas when proving actual OTP
//
// All of the otp-related lemmas should be pretty straightforward to discharge using xor

// $a \oplus b = b \oplus a$
lemma {:induction false} XorBitCommutative(a: bool, b: bool)
    ensures XorBit(a, b) == XorBit(b, a)
{
    calc {
        XorBit(a, b);
    ==
        a != b;
    ==
        b != a;
    ==
        XorBit(b, a);
    }
}

// $(a \oplus b) \oplus c = a \oplus (b \oplus c)$
lemma {:induction false} XorBitAssociative(a: bool, b: bool, c: bool)
    ensures XorBit(XorBit(a, b), c) == XorBit(a, XorBit(b, c))
{
    // Truth table verification for all 8 cases
    if a && b && c {
        calc {
            XorBit(XorBit(a, b), c);
        ==
            XorBit(false, true);
        ==
            true;
        ==
            XorBit(true, false);
        ==
            XorBit(a, XorBit(b, c));
        }
    } else if a && b && !c {
        calc {
            XorBit(XorBit(a, b), c);
        ==
            XorBit(false, false);
        ==
            false;
        ==
            XorBit(true, true);
        ==
            XorBit(a, XorBit(b, c));
        }
    } else if a && !b && c {
        assert XorBit(a, b) == true;
        assert XorBit(b, c) == true;
        calc {
            XorBit(XorBit(a, b), c);
        ==
            XorBit(true, true);
        ==
            false;
        }
        calc {
            XorBit(a, XorBit(b, c));
        ==
            XorBit(true, true);
        ==
            false;
        }
    } else if a && !b && !c {
        assert XorBit(a, b) == true;
        assert XorBit(b, c) == false;
        calc {
            XorBit(XorBit(a, b), c);
        ==
            XorBit(true, false);
        ==
            true;
        }
        calc {
            XorBit(a, XorBit(b, c));
        ==
            XorBit(true, false);
        ==
            true;
        }
    } else if !a && b && c {
        calc {
            XorBit(XorBit(a, b), c);
        ==
            XorBit(true, true);
        ==
            false;
        ==
            XorBit(false, false);
        ==
            XorBit(a, XorBit(b, c));
        }
    } else if !a && b && !c {
        calc {
            XorBit(XorBit(a, b), c);
        ==
            XorBit(true, false);
        ==
            true;
        ==
            XorBit(false, true);
        ==
            XorBit(a, XorBit(b, c));
        }
    } else if !a && !b && c {
        calc {
            XorBit(XorBit(a, b), c);
        ==
            XorBit(false, true);
        ==
            true;
        ==
            XorBit(false, true);
        ==
            XorBit(a, XorBit(b, c));
        }
    } else {
        calc {
            XorBit(XorBit(a, b), c);
        ==
            XorBit(false, false);
        ==
            false;
        ==
            XorBit(false, false);
        ==
            XorBit(a, XorBit(b, c));
        }
    }
}

// $a \oplus 0 = a$
lemma {:induction false} XorBitIdentity(a: bool)
    ensures XorBit(a, false) == a
{
    calc {
        XorBit(a, false);
    ==
        a != false;
    ==
        a;
    }
}

// $(a \oplus b) \oplus b = a$
lemma {:induction false} XorBitSelfInverse(a: bool, b: bool)
    ensures XorBit(XorBit(a, b), b) == a
{
    XorBitAssociative(a, b, b);
    calc {
        XorBit(XorBit(a, b), b);
    ==  { XorBitAssociative(a, b, b); }
        XorBit(a, XorBit(b, b));
    ==
        XorBit(a, (b != b));
    ==
        XorBit(a, false);
    ==  { XorBitIdentity(a); }
        a;
    }
}

// Helper: $|\vec{0}_n| = n$
lemma {:induction false} ZerosLength(n: nat)
    ensures |Zeros(n)| == n
    decreases n
{
    if n == 0 {
    } else {
        ZerosLength(n - 1);
        calc {
            |Zeros(n)|;
        ==
            |[false] + Zeros(n - 1)|;
        ==
            1 + |Zeros(n - 1)|;
        ==
            1 + (n - 1);
        ==
            n;
        }
    }
}

// Length of $a \oplus b$ equals length of $a$
lemma {:induction false} XorSeqLength(a: seq<bool>, b: seq<bool>)
    requires |a| == |b|
    ensures |XorSeq(a, b)| == |a|
    decreases |a|
{
    if |a| == 0 {
    } else {
        XorSeqLength(a[1..], b[1..]);
        calc {
            |XorSeq(a, b)|;
        ==
            |[XorBit(a[0], b[0])] + XorSeq(a[1..], b[1..])|;
        ==
            1 + |XorSeq(a[1..], b[1..])|;
        ==
            1 + |a[1..]|;
        ==
            |a|;
        }
    }
}

// $a \oplus b = b \oplus a$ for sequences
lemma {:induction false} XorSeqCommutative(a: seq<bool>, b: seq<bool>)
    requires |a| == |b|
    ensures XorSeq(a, b) == XorSeq(b, a)
    decreases |a|
{
    if |a| == 0 {
    } else {
        XorBitCommutative(a[0], b[0]);
        XorSeqCommutative(a[1..], b[1..]);
        calc {
            XorSeq(a, b);
        ==
            [XorBit(a[0], b[0])] + XorSeq(a[1..], b[1..]);
        ==
            [XorBit(b[0], a[0])] + XorSeq(b[1..], a[1..]);
        ==
            XorSeq(b, a);
        }
    }
}

// $(a \oplus b) \oplus c = a \oplus (b \oplus c)$ for sequences
lemma {:induction false} XorSeqAssociative(a: seq<bool>, b: seq<bool>, c: seq<bool>)
    requires |a| == |b| == |c|
    ensures |XorSeq(a, b)| == |c|
    ensures |XorSeq(b, c)| == |a|
    ensures XorSeq(XorSeq(a, b), c) == XorSeq(a, XorSeq(b, c))
    decreases |a|
{
    if |a| == 0 {
    } else {
        XorBitAssociative(a[0], b[0], c[0]);
        XorSeqAssociative(a[1..], b[1..], c[1..]);
        XorSeqLength(a, b);
        XorSeqLength(b, c);
        calc {
            XorSeq(XorSeq(a, b), c);
        ==
            XorSeq([XorBit(a[0], b[0])] + XorSeq(a[1..], b[1..]), c);
        ==
            [XorBit(XorBit(a[0], b[0]), c[0])] + XorSeq(XorSeq(a[1..], b[1..]), c[1..]);
        ==
            [XorBit(a[0], XorBit(b[0], c[0]))] + XorSeq(a[1..], XorSeq(b[1..], c[1..]));
        ==
            XorSeq(a, [XorBit(b[0], c[0])] + XorSeq(b[1..], c[1..]));
        ==
            XorSeq(a, XorSeq(b, c));
        }
    }
}

// $a \oplus \vec{0} = a$ for sequences
lemma {:induction false} XorSeqIdentity(a: seq<bool>)
    ensures |Zeros(|a|)| == |a|
    ensures XorSeq(a, Zeros(|a|)) == a
    decreases |a|
{
    ZerosLength(|a|);
    if |a| == 0 {
    } else {
        XorBitIdentity(a[0]);
        XorSeqIdentity(a[1..]);
        ZerosLength(|a| - 1);
        calc {
            XorSeq(a, Zeros(|a|));
        ==
            XorSeq(a, [false] + Zeros(|a| - 1));
        ==
            [XorBit(a[0], false)] + XorSeq(a[1..], Zeros(|a| - 1));
        ==
            [a[0]] + XorSeq(a[1..], Zeros(|a[1..]|));
        ==
            [a[0]] + a[1..];
        ==
            a;
        }
    }
}

// $(a \oplus b) \oplus b = a$ for sequences
lemma {:induction false} XorSeqSelfInverse(a: seq<bool>, b: seq<bool>)
    requires |a| == |b|
    ensures |XorSeq(a, b)| == |b|
    ensures XorSeq(XorSeq(a, b), b) == a
    decreases |a|
{
    if |a| == 0 {
    } else {
        XorBitSelfInverse(a[0], b[0]);
        XorSeqSelfInverse(a[1..], b[1..]);
        XorSeqLength(a, b);
        calc {
            XorSeq(XorSeq(a, b), b);
        ==
            XorSeq([XorBit(a[0], b[0])] + XorSeq(a[1..], b[1..]), b);
        ==
            [XorBit(XorBit(a[0], b[0]), b[0])] + XorSeq(XorSeq(a[1..], b[1..]), b[1..]);
        ==
            [a[0]] + a[1..];
        ==
            a;
        }
    }
}

// Ciphertext preserves plaintext length
lemma {:induction false} OtpEncryptPreservesLength(plaintext: seq<bool>, key: seq<bool>)
    requires |plaintext| == |key|
    ensures |OtpEncrypt(plaintext, key)| == |plaintext|
{
}

// Ciphertext has same length as key
lemma {:induction false} OtpCiphertextKeyLength(plaintext: seq<bool>, key: seq<bool>)
    requires |plaintext| == |key|
    ensures |OtpEncrypt(plaintext, key)| == |key|
{

}

// Decryption preserves ciphertext length
lemma {:induction false} OtpDecryptPreservesLength(ciphertext: seq<bool>, key: seq<bool>)
    requires |ciphertext| == |key|
    ensures |OtpDecrypt(ciphertext, key)| == |ciphertext|
{
}

// $\text{OtpDecrypt}(\text{OtpEncrypt}(m, k), k) = m$
lemma {:induction false} OtpEncryptDecryptCorrectness(plaintext: seq<bool>, key: seq<bool>)
    requires |plaintext| == |key|
    ensures |OtpEncrypt(plaintext, key)| == |key|
    ensures OtpDecrypt(OtpEncrypt(plaintext, key), key) == plaintext
{
}

// $\text{OtpEncrypt}(c, k) = \text{OtpDecrypt}(c, k)$
lemma {:induction false} OtpEncryptDecryptSymmetric(message: seq<bool>, key: seq<bool>)
    requires |message| == |key|
    ensures OtpEncrypt(message, key) == OtpDecrypt(message, key)
{
}

// $\text{OtpEncrypt}(\text{OtpEncrypt}(m, k), k) = m$
lemma {:induction false} OtpDoubleEncryptIdentity(plaintext: seq<bool>, key: seq<bool>)
    requires |plaintext| == |key|
    ensures |OtpEncrypt(plaintext, key)| == |key|
    ensures OtpEncrypt(OtpEncrypt(plaintext, key), key) == plaintext
{
}

// $\text{OtpEncrypt}(\text{OtpEncrypt}(m, k_1), k_2) = \text{OtpEncrypt}(\text{OtpEncrypt}(m, k_2), k_1)$
lemma {:induction false} OtpEncryptCommutative(plaintext: seq<bool>, key1: seq<bool>, key2: seq<bool>)
    requires |plaintext| == |key1| == |key2|
    ensures |OtpEncrypt(plaintext, key1)| == |key2|
    ensures |OtpEncrypt(plaintext, key2)| == |key1|
    ensures OtpEncrypt(OtpEncrypt(plaintext, key1), key2) == OtpEncrypt(OtpEncrypt(plaintext, key2), key1)
{
}

// $\text{OtpEncrypt}(m, k_1 \oplus k_2) = \text{OtpEncrypt}(\text{OtpEncrypt}(m, k_1), k_2)$
lemma {:induction false} OtpCombinedKeyEncryption(plaintext: seq<bool>, key1: seq<bool>, key2: seq<bool>)
    requires |plaintext| == |key1| == |key2|
    ensures |XorSeq(key1, key2)| == |plaintext|
    ensures |OtpEncrypt(plaintext, key1)| == |key2|
    ensures OtpEncrypt(plaintext, XorSeq(key1, key2)) == OtpEncrypt(OtpEncrypt(plaintext, key1), key2)
{
}

// For any plaintext $m$ and ciphertext $c$, encrypting $m$ with $k = m \oplus c$ yields $c$
lemma {:induction false} KeyForSound(plaintext: seq<bool>, ciphertext: seq<bool>)
    requires |plaintext| == |ciphertext|
    ensures |KeyFor(plaintext, ciphertext)| == |plaintext|
    ensures OtpEncrypt(plaintext, KeyFor(plaintext, ciphertext)) == ciphertext
{
}

// If $m \oplus k = c$, then $k = m \oplus c$
lemma {:induction false} KeyForUnique(plaintext: seq<bool>, ciphertext: seq<bool>, key: seq<bool>)
    requires |plaintext| == |ciphertext| == |key|
    requires OtpEncrypt(plaintext, key) == ciphertext
    ensures key == KeyFor(plaintext, ciphertext)
{
}

