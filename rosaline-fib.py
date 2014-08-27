with open("C:/python/data.txt") as f:
	c = f.readlines()

def fibk(n, k):
    if n == 1: return 1
    elif n == 2: return 1
    else: return (k * fibk(n-2, k)) + fibk(n-1, k)

print (fibk(34, 3))
