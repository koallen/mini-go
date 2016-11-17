func fac(n int) {
    if n > 1 {
        print n;
        fac(n-1)
    } else {
        print n
    }
}

{
    fac(3)
}
