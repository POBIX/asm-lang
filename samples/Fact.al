// Recursively calculates the factorial of a number

func factorial(n: int) -> int
{
    local d: int;
    if n == 0 { return 1; }
    math d = n - 1;
    set d = factorial(d);
    math d = d * n;
    return d;
}

func main() -> int
{
    // due to 16-bit limitations, n cannot be larger than 8.
    local n: int = 8;
    call print_int(n);
    call print("! = ");
    call print_int(factorial(n));
    return 0;
}
