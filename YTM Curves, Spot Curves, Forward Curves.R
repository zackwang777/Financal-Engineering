rawdata <- read.csv("/Users/zack/Desktop/Data.csv")

#Dirty Price
dirty_price <- function(p, n, c) {p + ((183-n)/365) * (c)}
dp <- NULL
for (i in 1:nrow(rawdata))
{
  a <- dirty_price(rawdata$Price[i], rawdata$Number.of.Days.to.Next.Payment[i], rawdata$Coupon[i])
  dp <- c(dp, a)
}
data <- cbind(rawdata, dp)

#rate_half
rate_half <- c(NULL)
for (i in 1:10)
{
  y <- function(r)
  {
    (data[i,5]/2 + 100) * exp(-r * (data[i, 8]/365)) - data[i,9]
  }
  r <- uniroot(y, c(-20000,20000))$root
  rate_half <- c(rate_half,r)
}
rate_half

#rate_one
rate_one <- c(NULL)
for (i in 11:20)
{
  y <- function(r)
  {
    (data[i,5]/2) * exp(-r * (data[i, 8]/365)) + (data[i,5]/2 + 100) * exp(-r * ((data[i, 8]/365) + 0.5)) - data[i,9]
  }
  r <- uniroot(y, c(-20000,20000))$root
  rate_one <- c(rate_one,r)
}
rate_one

#rate_two_thirds
rate_two_thirds <- c(NULL)
for (i in 21:30)
{
  y <- function(r)
  {
    (data[i,5]/2) * exp(-r * (data[i, 8]/365)) + (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 0.5)) + (data[i,5]/2 + 100) * exp(-r * ((data[i, 8]/365) + 1)) - data[i,9]
  }
  r <- uniroot(y, c(-20000,20000))$root
  rate_two_thirds <- c(rate_two_thirds,r)
}
rate_two_thirds

#rate_two
rate_two <- c(NULL)
for (i in 31:40)
{
  y <- function(r)
  {
    (data[i,5]/2) * exp(-r * (data[i, 8]/365)) + (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 0.5)) + (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 1)) + (data[i,5]/2 + 100) * exp(-r * ((data[i, 8]/365) + 1.5)) - data[i,9]
  }
  r <- uniroot(y, c(-20000,20000))$root
  rate_two <- c(rate_two,r)
}
rate_two

#rate_two_fifths
rate_two_fifths <- c(NULL)
for (i in 41:50)
{
  y <- function(r)
  {
    (data[i,5]/2) * exp(-r * (data[i, 8]/365)) + (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 0.5)) + (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 1)) + (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 1.5)) + (data[i,5]/2 + 100) * exp(-r * ((data[i, 8]/365) + 2)) - data[i,9]
  }
  r <- uniroot(y, c(-20000,20000))$root
  rate_two_fifths <- c(rate_two_fifths,r)
}
rate_two_fifths

#rate_three
rate_three <- c(NULL)
for (i in 51:60)
{
  y <- function(r)
  {
    (data[i,5]/2) * exp(-r * (data[i, 8]/365)) + (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 0.5)) + 
      (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 1)) + (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 1.5)) + 
      (data[i,5]/2 + 100) * exp(-r * ((data[i, 8]/365) + 2)) - data[i,9]
  }
  r <- uniroot(y, c(-20000,20000))$root
  rate_three <- c(rate_three,r)
}
rate_three

#rate_two_sevenths
rate_two_sevenths <- c(NULL)
for (i in 61:70)
{
  y <- function(r)
  {
    (data[i,5]/2) * exp(-r * (data[i, 8]/365)) + (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 0.5)) + 
      (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 1)) + (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 1.5)) + 
      (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 2)) + (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 2.5)) + 
      (data[i,5]/2 + 100) * exp(-r * ((data[i, 8]/365) + 3)) - data[i,9]
  }
  r <- uniroot(y, c(-20000,20000))$root
  rate_two_sevenths <- c(rate_two_sevenths,r)
}
rate_two_sevenths

#rate_four
rate_four <- c(NULL)
for (i in 71:80)
{
  y <- function(r)
  {
    (data[i,5]/2) * exp(-r * (data[i, 8]/365)) + (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 0.5)) + 
      (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 1)) + (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 1.5)) + 
      (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 2)) + (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 2.5)) + 
      (data[i,5]/2 + 100) * exp(-r * ((data[i, 8]/365) + 3)) - data[i,9]
  }
  r <- uniroot(y, c(-20000,20000))$root
  rate_four <- c(rate_four,r)
}
rate_four

#rate_two_ninths
rate_two_ninths <- c(NULL)
for (i in 81:90)
{
  y <- function(r)
  {
    (data[i,5]/2) * exp(-r * (data[i, 8]/365)) + (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 0.5)) + 
      (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 1)) + (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 1.5)) + 
      (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 2)) + (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 2.5)) + 
      (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 3)) + (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 3.5)) +
      (data[i,5]/2 + 100) * exp(-r * ((data[i, 8]/365) + 4)) - data[i,9]
  }
  r <- uniroot(y, c(-20000,20000))$root
  rate_two_ninths <- c(rate_two_ninths,r)
}
rate_two_ninths

#rate_five
rate_five <- c(NULL)
for (i in 91:100)
{
  y <- function(r)
  {
    (data[i,5]/2) * exp(-r * (data[i, 8]/365)) + (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 0.5)) + 
      (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 1)) + (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 1.5)) + 
      (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 2)) + (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 2.5)) + 
      (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 3)) + (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 3.5)) +
      (data[i,5]/2) * exp(-r * ((data[i, 8]/365) + 4)) + (data[i,5]/2 + 100) * exp(-r * ((data[i, 8]/365) + 4.5)) - data[i,9]
  }
  r <- uniroot(y, c(-20000,20000))$root
  rate_five <- c(rate_five,r)
}
rate_five

all_rate <- cbind(rate_half,rate_one,rate_two_thirds,rate_two,rate_two_fifths,rate_three,rate_two_sevenths,rate_four,rate_two_ninths,rate_five)
all_rate
x <- c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)
y <- all_rate[1,1:10]
plot(x,y, type = "o", col = "red", xlab = "Years", ylab = "Yield Rate", main = "Yield Curve", ylim = c(0.014,0.027))
lines(x,all_rate[2,1:10], type = "o", col = "blue")
lines(x,all_rate[3,1:10], type = "o", col = "yellow")
lines(x,all_rate[4,1:10], type = "o", col = "green")
lines(x,all_rate[5,1:10], type = "o", col = "black")
lines(x,all_rate[6,1:10], type = "o", col = "orange")
lines(x,all_rate[7,1:10], type = "o", col = "pink")
lines(x,all_rate[8,1:10], type = "o", col = "brown")
lines(x,all_rate[9,1:10], type = "o", col = "gold")
lines(x,all_rate[10,1:10], type = "o", col = "purple")
legend(3,0.027,c("1/2/2020","1/3/2020", "1/6/2020", "1/7/2020", "1/8/2020", "1/9/2020", "1/10/2020", "1/13/2020", "1/14/2020", "1/15/2020"), lwd=c(2,2,2,2,2,2,2,2,2,2), col=c("red", "blue", "yellow", "green", "black", "orange", "pink", "brown", "gold", "purple"), y.intersp=0.7)


#Spot Curve

#srate_half
srate_half <- c(NULL)
for (i in 1:10)
{
  y <- function(r)
  {
    (data[i,5]/2 + 100) * exp(-r * (data[i, 8]/365)) - data[i,9]
  }
  r <- uniroot(y, c(-20000,20000))$root
  srate_half <- c(srate_half,r)
}
srate_half

#srate_one
srate_one <- c(NULL)
for (i in 11:20)
{
  y <- function(r)
  {
    (data[i,5]/2) * exp(-srate_half[i-10] * (data[i, 8]/365)) + (data[i,5]/2 + 100) * exp(-r * ((data[i, 8]/365) + 0.5)) - data[i,9]
  }
  r <- uniroot(y, c(-20000,20000))$root
  srate_one <- c(srate_one,r)
}
srate_one

#srate_two_thirds
srate_two_thirds <- c(NULL)
for (i in 21:30)
{
  y <- function(r)
  {
    (data[i,5]/2) * exp(-srate_half[i-20] * (data[i, 8]/365)) + (data[i,5]/2) * exp(-srate_one[i-20] * ((data[i, 8]/365) + 0.5)) + (data[i,5]/2 + 100) * exp(-r * ((data[i, 8]/365) + 1)) - data[i,9]
  }
  r <- uniroot(y, c(-20000,20000))$root
  srate_two_thirds <- c(srate_two_thirds,r)
}
srate_two_thirds

#srate_two
srate_two <- c(NULL)
for (i in 31:40)
{
  y <- function(r)
  {
    (data[i,5]/2) * exp(-srate_half[i-30] * (data[i, 8]/365)) + (data[i,5]/2) * exp(-srate_one[i-30] * ((data[i, 8]/365) + 0.5)) + 
      (data[i,5]/2) * exp(-srate_two_thirds[i-30] * ((data[i, 8]/365) + 1)) + 
      (data[i,5]/2 + 100) * exp(-r * ((data[i, 8]/365) + 1.5)) - data[i,9]
  }
  r <- uniroot(y, c(-20000,20000))$root
  srate_two <- c(srate_two,r)
}
srate_two

#srate_two_fifths
srate_two_fifths <- c(NULL)
for (i in 41:50)
{
  y <- function(r)
  {
    (data[i,5]/2) * exp(-srate_half[i-40] * (data[i, 8]/365)) + (data[i,5]/2) * exp(-srate_one[i-40] * ((data[i, 8]/365) + 0.5)) + 
      (data[i,5]/2) * exp(-srate_two_thirds[i-40] * ((data[i, 8]/365) + 1)) + 
      (data[i,5]/2) * exp(-srate_two[i-40] * ((data[i, 8]/365) + 1.5)) + (data[i,5]/2 + 100) * exp(-r * ((data[i, 8]/365) + 2)) - data[i,9]
  }
  r <- uniroot(y, c(-20000,20000))$root
  srate_two_fifths <- c(srate_two_fifths,r)
}
srate_two_fifths

#srate_three
srate_three <- c(NULL)
for (i in 51:60)
{
  y <- function(r)
  {
    (data[i,5]/2) * exp(-srate_half[i-50] * (data[i, 8]/365)) + (data[i,5]/2) * exp(-srate_one[i-50] * ((data[i, 8]/365) + 0.5)) + 
      (data[i,5]/2) * exp(-srate_two_thirds[i-50] * ((data[i, 8]/365) + 1)) + 
      (data[i,5]/2) * exp(-srate_two[i-50] * ((data[i, 8]/365) + 1.5)) + (data[i,5]/2 + 100) * exp(-r * ((data[i, 8]/365) + 2)) - data[i,9]
  }
  r <- uniroot(y, c(-20000,20000))$root
  srate_three <- c(srate_three,r)
}
srate_three

#srate_two_sevenths
srate_two_sevenths <- c(NULL)
for (i in 61:70)
{
  y <- function(r)
  {
    (data[i,5]/2) * exp(-srate_half[i-60] * (data[i, 8]/365)) + (data[i,5]/2) * exp(-srate_one[i-60] * ((data[i, 8]/365) + 0.5)) + 
      (data[i,5]/2) * exp(-srate_two_thirds[i-60] * ((data[i, 8]/365) + 1)) + (data[i,5]/2) * exp(-srate_two[i-60] * ((data[i, 8]/365) + 1.5)) + 
      (data[i,5]/2) * exp(-srate_two_fifths[i-60] * ((data[i, 8]/365) + 2)) + (data[i,5]/2) * exp(-srate_three[i-60] * ((data[i, 8]/365) + 2.5)) + 
      (data[i,5]/2 + 100) * exp(-r * ((data[i, 8]/365) + 3)) - data[i,9]
  }
  r <- uniroot(y, c(-20000,20000))$root
  srate_two_sevenths <- c(srate_two_sevenths,r)
}
srate_two_sevenths

#srate_four
srate_four <- c(NULL)
for (i in 71:80)
{
  y <- function(r)
  {
    (data[i,5]/2) * exp(-srate_half[i-70] * (data[i, 8]/365)) + (data[i,5]/2) * exp(-srate_one[i-70] * ((data[i, 8]/365) + 0.5)) + 
      (data[i,5]/2) * exp(-srate_two_thirds[i-70] * ((data[i, 8]/365) + 1)) + (data[i,5]/2) * exp(-srate_two[i-70] * ((data[i, 8]/365) + 1.5)) + 
      (data[i,5]/2) * exp(-srate_two_fifths[i-70] * ((data[i, 8]/365) + 2)) + (data[i,5]/2) * exp(-srate_three[i-70] * ((data[i, 8]/365) + 2.5)) + 
      (data[i,5]/2 + 100) * exp(-r * ((data[i, 8]/365) + 3)) - data[i,9]
  }
  r <- uniroot(y, c(-20000,20000))$root
  srate_four <- c(srate_four,r)
}
srate_four

#srate_two_ninths
srate_two_ninths <- c(NULL)
for (i in 81:90)
{
  y <- function(r)
  {
    (data[i,5]/2) * exp(-srate_half[i-80] * (data[i, 8]/365)) + (data[i,5]/2) * exp(-srate_one[i-80] * ((data[i, 8]/365) + 0.5)) + 
      (data[i,5]/2) * exp(-srate_two_thirds[i-80] * ((data[i, 8]/365) + 1)) + (data[i,5]/2) * exp(-srate_two[i-80] * ((data[i, 8]/365) + 1.5)) + 
      (data[i,5]/2) * exp(-srate_two_fifths[i-80] * ((data[i, 8]/365) + 2)) + (data[i,5]/2) * exp(-srate_three[i-80] * ((data[i, 8]/365) + 2.5)) + 
      (data[i,5]/2) * exp(-srate_two_sevenths[i-80] * ((data[i, 8]/365) + 3)) + (data[i,5]/2) * exp(-srate_four[i-80] * ((data[i, 8]/365) + 3.5)) + 
      (data[i,5]/2 + 100) * exp(-r * ((data[i, 8]/365) + 4)) - data[i,9]
  }
  r <- uniroot(y, c(-20000,20000))$root
  srate_two_ninths <- c(srate_two_ninths,r)
}
srate_two_ninths

#srate_five
srate_five <- c(NULL)
for (i in 91:100)
{
  y <- function(r)
  {
    (data[i,5]/2) * exp(-srate_half[i-90] * (data[i, 8]/365)) + (data[i,5]/2) * exp(-srate_one[i-90] * ((data[i, 8]/365) + 0.5)) + 
      (data[i,5]/2) * exp(-srate_two_thirds[i-90] * ((data[i, 8]/365) + 1)) + (data[i,5]/2) * exp(-srate_two[i-90] * ((data[i, 8]/365) + 1.5)) + 
      (data[i,5]/2) * exp(-srate_two_fifths[i-90] * ((data[i, 8]/365) + 2)) + (data[i,5]/2) * exp(-srate_three[i-90] * ((data[i, 8]/365) + 2.5)) + 
      (data[i,5]/2) * exp(-srate_two_sevenths[i-90] * ((data[i, 8]/365) + 3)) + (data[i,5]/2) * exp(-srate_four[i-90] * ((data[i, 8]/365) + 3.5)) + 
      (data[i,5]/2) * exp(-srate_two_ninths[i-90] * ((data[i, 8]/365) + 4)) + (data[i,5]/2 + 100) * exp(-r * ((data[i, 8]/365) + 4.5)) - data[i,9]
  }
  r <- uniroot(y, c(-20000,20000))$root
  srate_five <- c(srate_five,r)
}
srate_five

all_srate <- cbind(srate_half,srate_one,srate_two_thirds,srate_two,srate_two_fifths,srate_three,srate_two_sevenths,srate_four,srate_two_ninths,srate_five)
all_srate
x <- c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)
y <- all_srate[1,1:10]
plot(x,y, type = "o", col = "red", xlab = "Years", ylab = "Spot Rate", main = "Spot Curve", ylim = c(0.014,0.027))
lines(x,all_srate[2,1:10], type = "o", col = "blue")
lines(x,all_srate[3,1:10], type = "o", col = "yellow")
lines(x,all_srate[4,1:10], type = "o", col = "green")
lines(x,all_srate[5,1:10], type = "o", col = "black")
lines(x,all_srate[6,1:10], type = "o", col = "orange")
lines(x,all_srate[7,1:10], type = "o", col = "pink")
lines(x,all_srate[8,1:10], type = "o", col = "brown")
lines(x,all_srate[9,1:10], type = "o", col = "gold")
lines(x,all_srate[10,1:10], type = "o", col = "purple")
legend(3,0.027,c("1/2/2020","1/3/2020", "1/6/2020", "1/7/2020", "1/8/2020", "1/9/2020", "1/10/2020", "1/13/2020", "1/14/2020", "1/15/2020"), lwd=c(2,2,2,2,2,2,2,2,2,2), col=c("red", "blue", "yellow", "green", "black", "orange", "pink", "brown", "gold", "purple"), y.intersp=0.7)


#Forward Curve

#1yr-1yr
one_yr <- c(NULL)
for (i in 1:10)
{
  f <- (((1+srate_two[i])^2)/(1+srate_one[i]))^(1/1) - 1
  one_yr <- c(one_yr,f)
}
one_yr

#1yr-2yr
two_yr <- c(NULL)
for (i in 1:10)
{
  f <- (((1+srate_three[i])^3)/(1+srate_one[i]))^(1/2) - 1
  two_yr <- c(two_yr,f)
}
two_yr

#1yr-3yr
three_yr <- c(NULL)
for (i in 1:10)
{
  f <- (((1+srate_four[i])^4)/(1+srate_one[i]))^(1/3) - 1
  three_yr <- c(three_yr,f)
}
three_yr

#1yr-4yr
four_yr <- c(NULL)
for (i in 1:10)
{
  f <- (((1+srate_five[i])^5)/(1+srate_one[i]))^(1/4) - 1
  four_yr <- c(four_yr,f)
}
four_yr

all_frate <- cbind(one_yr, two_yr, three_yr, four_yr)
all_frate
yrs <- c(1,2,3,4)
a <- all_frate[1,1:4]
plot(yrs,a, type = "o", col = "red", xlab = "Years", ylab = "Forward Rate", main = "Forward Curve", ylim = c(0.014,0.024))
lines(yrs,all_frate[2,1:4], type = "o", col = "blue")
lines(yrs,all_frate[3,1:4], type = "o", col = "yellow")
lines(yrs,all_frate[4,1:4], type = "o", col = "green")
lines(yrs,all_frate[5,1:4], type = "o", col = "black")
lines(yrs,all_frate[6,1:4], type = "o", col = "orange")
lines(yrs,all_frate[7,1:4], type = "o", col = "pink")
lines(yrs,all_frate[8,1:4], type = "o", col = "brown")
lines(yrs,all_frate[9,1:4], type = "o", col = "gold")
lines(yrs,all_frate[10,1:4], type = "o", col = "purple")
legend(2,0.024,c("1/2/2020","1/3/2020", "1/6/2020", "1/7/2020", "1/8/2020", "1/9/2020", "1/10/2020", "1/13/2020", "1/14/2020", "1/15/2020"), lwd=c(2,2,2,2,2,2,2,2,2,2), col=c("red", "blue", "yellow", "green", "black", "orange", "pink", "brown", "gold", "purple"), y.intersp=0.7)

#cov
x_1 <- c(NULL)
for (i in 1:9)
{
  x <- log(rate_one[i+1]/rate_one[i])
  x_1 <- c(x_1,x)
}
x_1

x_2 <- c(NULL)
for (i in 1:9)
{
  x <- log(rate_two[i+1]/rate_two[i])
  x_2 <- c(x_2,x)
}
x_2

x_3 <- c(NULL)
for (i in 1:9)
{
  x <- log(rate_three[i+1]/rate_three[i])
  x_3 <- c(x_3,x)
}
x_3

x_4 <- c(NULL)
for (i in 1:9)
{
  x <- log(rate_four[i+1]/rate_four[i])
  x_4 <- c(x_4,x)
}
x_4

x_5 <- c(NULL)
for (i in 1:9)
{
  x <- log(rate_five[i+1]/rate_five[i])
  x_5 <- c(x_5,x)
}
x_5

x <- cbind(x_1,x_2,x_3,x_4,x_5)
cov(x)

f_1 <- c(NULL)
for (i in 1:9)
{
  f <- log(one_yr[i+1]/one_yr[i])
  f_1 <- c(f_1,f)
}
f_1

f_2 <- c(NULL)
for (i in 1:9)
{
  f <- log(two_yr[i+1]/two_yr[i])
  f_2 <- c(f_2,f)
}
f_2

f_3 <- c(NULL)
for (i in 1:9)
{
  f <- log(three_yr[i+1]/three_yr[i])
  f_3 <- c(f_3,f)
}
f_3

f_4 <- c(NULL)
for (i in 1:9)
{
  f <- log(four_yr[i+1]/four_yr[i])
  f_4 <- c(f_4,f)
}
f_4

f <- cbind(f_1,f_2,f_3,f_4)
cov(f)

#eigen
eigen(cov(x))
eigen(cov(f))

