with open("C:/python/data.txt") as f:
	c = f.readlines()

c = c[0]	
print(c.count('A'), c.count('C'), c.count('G'), c.count('T'))

