def collatz_sequence_length(n, cache):
    if n in cache:
        return cache[n]
    if n == 1:
        return 1
    if n % 2 == 0:
        next_n = n // 2
    else:
        next_n = 3 * n + 1
    length = 1 + collatz_sequence_length(next_n, cache)
    cache[n] = length
    return length

def solve_problem_14(limit):
    cache = {}
    max_length = 0
    starting_number = 0
    
    for i in range(1, limit):
        length = collatz_sequence_length(i, cache)
        if length > max_length:
            max_length = length
            starting_number = i
    
    return starting_number

if __name__ == "__main__":
    result = solve_problem_14(1_000_000)
    print(f"The starting number under 1,000,000 that produces the longest Collatz sequence is: {result}")
