with open("C:/python/data.txt") as f:
	c = f.readlines()

def rep(x):
    if x == 'A': return 'T'
    if x == 'C': return 'G'
    if x == 'G': return 'C'
    if x == 'T': return 'A'

c = c[0]
print (''.join([rep(x) for x in c[::-1]]))
