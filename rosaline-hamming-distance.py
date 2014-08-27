with open("C:/python/data.txt") as f:
	c = f.readlines()

def dH(s,t):
    if len(s) == 0: return 0
    elif s[0] != t[0]: return 1 + dH(s[1:], t[1:])
    else: return 0 + dH(s[1:], t[1:])

c = [s.strip() for s in c]
print (c[0])
print (c[1])
print (dH(c[0], c[1]))
