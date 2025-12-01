function {:induction false} XorBit(a: bool, b: bool): bool
{
    a != b
}

function {:induction false} XorSeq(a: seq<bool>, b: seq<bool>): seq<bool>
    requires |a| == |b|
    decreases |a|
{
    if |a| == 0 then []
    else [XorBit(a[0], b[0])] + XorSeq(a[1..], b[1..])
}

function {:induction false} Zeros(n: nat): seq<bool>
    decreases n
{
    if n == 0 then []
    else [false] + Zeros(n - 1)
}

function {:induction false} OtpEncrypt(plaintext: seq<bool>, key: seq<bool>): seq<bool>
    requires |plaintext| == |key|
{
    XorSeq(plaintext, key)
}

function {:induction false} OtpDecrypt(ciphertext: seq<bool>, key: seq<bool>): seq<bool>
    requires |ciphertext| == |key|
{
    XorSeq(ciphertext, key)
}

function {:induction false} KeyFor(plaintext: seq<bool>, ciphertext: seq<bool>): seq<bool>
    requires |plaintext| == |ciphertext|
{
    XorSeq(plaintext, ciphertext)
}

predicate {:induction false} IsOtpCipher(plaintext: seq<bool>, key: seq<bool>, ciphertext: seq<bool>)
    requires |plaintext| == |key|
{
    |ciphertext| == |plaintext| && ciphertext == OtpEncrypt(plaintext, key)
}


method {:induction false} CryptoTests(plaintext: seq<bool>, key: seq<bool>, otherKey: seq<bool>, ciphertext: seq<bool>)
    requires |plaintext| == |key|
    requires |plaintext| == |otherKey|
    requires |plaintext| == |ciphertext|
    requires IsOtpCipher(plaintext, key, ciphertext)
{
    OtpEncryptPreservesLength(plaintext, key);
    OtpCiphertextKeyLength(plaintext, key);
    OtpDecryptPreservesLength(ciphertext, key);

    OtpEncryptDecryptCorrectness(plaintext, key);

    OtpDoubleEncryptIdentity(plaintext, key);
    assert OtpEncrypt(OtpEncrypt(plaintext, key), key) == plaintext;

    OtpEncryptCommutative(plaintext, key, otherKey);
    assert OtpEncrypt(OtpEncrypt(plaintext, key), otherKey) == OtpEncrypt(OtpEncrypt(plaintext, otherKey), key);

    OtpCombinedKeyEncryption(plaintext, key, otherKey);

    KeyForSound(plaintext, ciphertext);
    assert OtpEncrypt(plaintext, KeyFor(plaintext, ciphertext)) == ciphertext;

    KeyForUnique(plaintext, ciphertext, key);
    assert key == KeyFor(plaintext, ciphertext);
}
