import itertools

with open("C:/python/data.txt") as f:
	c = f.readlines()

def fact(n):
    if n == 1: return 1
    else: return n * fact(n-1)

def t2s(t):
    ' '.join(map(str, t))

def perms(n):
    map(t2s, [x for x in itertools.permutations([1, 2, 3], 3)])

l = ['R', 'V', 'L', 'F', 'N']
n = 4
print (fact(n))
[print (x) for x in map((lambda a: ''.join(map(str,a))), [x for x in itertools.product(l, repeat=n)])]
