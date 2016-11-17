func max(a int, b int) int {
    x := 0;
    if a > b {
        x = a
    } else {
        x = b
    };
    return x
}

func fac(n int) int {
    result := 0;
    if n > 0 {
        result = n * fac(n - 1)
    } else {
        result = 1
    };
    return result
}

{
    x := max(10, 100);
    print x;
    y := fac(7);
    print y
}
