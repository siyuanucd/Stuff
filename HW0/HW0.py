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
# -----------------  problem 6  -------------------------#
'''a. Simulate from this process with n=1000. Plot the resulting series. 
b. Repeat part (a) 200 times, storing the result in a 1000x200 matrix. 
Each column should correspond to a realization of the random process. 
c. Compute the mean of the 200 realizations at each time point (i=1,2,…,1000).
Plot the means. d. Plot the variance of the 200 realizations at each time 
point (i=1,2,…,1000). Plot the variances. e. Compute the mean of each of the
200 series across time points (j=1,2,…,200). Plot the means. 
f. Compute the variance of each of the 200 series across time points (j=1,2,…).
Plot the variances. g. Justify the results you have seen in 
parts b.--f. theoretically.'''

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

# -----------------  problem 7  -------------------------#
'''Monte Carlo integration.'''
def MCI(f,n):
    a = [f(random.uniform(0,1)) for i in range(n)]    
    return mean(a)

def f(z):
    return math.exp(-math.pow(z,2))

MCI(f,100000)  '''the expectation is about 0.74774419323398644'''


'''importance sampling'''




# -----------------  problem 8  -------------------------#
'''Simulate from the linear regression model with n=100. Use the bootstrap
 procedure to estimate the SE of  based on B=1000 bootstrap resamples.'''
 
beta = [1.2,0.3,-0.9]

def simReg(n,X,y):
    y = [X[i][0]*beta[0]+X[i][1]*beta[1]+X[i][2]*beta[2]+random.uniform(0,1) for i in range(n)]
    beta_hat = numpy.linalg.lstsq(X, y)[0]
    return(beta_hat)


def BS(n,B):
    x1 = [random.uniform(0,1) for i in range(n)]
    x2 = [random.uniform(0,1) for i in range(n)]
    X = numpy.vstack([np.ones(n), x1, x2]).T
    y = [X[i][0]*beta[0]+X[i][1]*beta[1]+X[i][2]*beta[2]+random.uniform(0,1) for i in range(n)]
    
    beta_hat= numpy.zeros((B,3))   
    
    for b in range(0,B):
        sp = [random.randint(0,n-1) for i in range(n)]
        beta_hat[b] = simReg(n,X[sp], [y[i] for i in sp])
    return(beta_hat.std(0))
    
BS(100,1000)

''' standard deviation of beta_hat is 0.0788, 0.1036, 0.0989

 