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

n = 5
print (fact(n))
[print (a) for a in (map((lambda y: ' '.join(map(str,y))), [x for x in itertools.permutations(list(range(1,n+1)),n)]))]
