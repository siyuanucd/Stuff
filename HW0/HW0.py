import re
import os
import sys
import random
import math
import numpy
import matplotlib

# --------------------  problem 1 -------------------- #
'''Write a program that prints the numbers from 1 to 100. But for multiples of
 three print "Fizz" instead of the number and for the multiples of five print "Buzz".
 For numbers which are multiples of both three and five print "FizzBuzz" '''
def printFizzBuzz(start,end):
     for i in range(start,end+1):
         if i%3==0 and i%5!=0:
            print("Fizz")
         elif i%5==0 and i%3!=0:
              print('Buzz')
         elif i%3==0 and i%5==0:
              print('FizzBuzz')
         else:
              print(i)

printFizzBuzz(1,100)

# --------------------   problem 2  --------------------#
'''Write a program that generates 10,000 uniform random numbers between 0 and
(call this ), and 10,000 uniform random numbers between 0 and 1 (call this ).
You will then have 10,000 pairs of random numbers.'''

x = list()
y = list()
for i in range(10000):
   x.append(random.uniform(0, 2*math.pi))
   y.append(random.uniform(0,1))
u = [y[i]*math.cos(x[i]) for i in range(10000)]
v = [y[i]*math.sin(x[i]) for i in range(10000)]
line = plt.plot(u,v)
matplotlib.scatter(u, v, c=z)
plt.show()