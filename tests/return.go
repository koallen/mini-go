func max(a int, b int) int {
    x := 0;
    if a > b {
        x = a
    } else {
        x = b
    };
    return x
}

{
    x := max(10, 100);
    print x
}
