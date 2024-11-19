def number_to_words(n):
    ones = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", 
             "sixteen", "seventeen", "eighteen", "nineteen"]
    tens = ["", "", "twenty", "thirty", "forty", "fifty", 
            "sixty", "seventy", "eighty", "ninety"]
    if n == 1000:
        return "one thousand"
    result = ""
    if n >= 100:
        result += ones[n // 100] + " hundred"
        if n % 100 != 0:
            result += " and "
    n %= 100
    if n >= 20:
        result += tens[n // 10]
        if n % 10 != 0:
            result += "-" + ones[n % 10]
    elif n >= 10:
        result += teens[n - 10]
    elif n > 0:
        result += ones[n]
    return result

def count_letters_in_words(limit):
    total_letters = 0
    for i in range(1, limit + 1):
        words = number_to_words(i)
        letters = len(words.replace(" ", "").replace("-", ""))
        total_letters += letters
    return total_letters

if __name__ == "__main__":
    result = count_letters_in_words(1000)
    print(f"The total number of letters used to write out the numbers from 1 to 1000 is: {result}")
