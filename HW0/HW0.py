# -*- coding: utf-8 -*-
"""
Spyder Editor

This temporary script file is located here:
C:\Users\siyuanzhou\.spyder2\.temp.py
"""

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
matplotlib.pyplot.scatter(u,v)
matplotlib.pyplot.show()
''' distribution is uniform on a circle'''

# -----------------  problem 3 is written in R ----------#
# -----------------  problem 4  -------------------------#

def AR(n):
    pho = 0.8
    y = list()
    y.append(0)
    for i in range(1,n):
        y.append(pho*y[i-1]+random.normalvariate(0,1))
    return y
y = AR(1000)
plot(range(1000),y,'g')
show()

m = numpy.zeros((200,1000))
for i in range(200):
    m[i,]=AR(1000)
m = m.T

subplot(1,2,1)
plot(range(200),m.mean(0),'g')
subplot(1,2,2)
plot(range(200),m.var(0),'g')
show()


subplot(1,2,1)
plot(range(1000),m.mean(1),'g')
subplot(1,2,2)
plot(range(1000),m.var(1),'g')
show()

