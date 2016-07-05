sphereFunction <- function(x)
{
  res = sum(x^2)

  return (res)
}


rosenbrockFunction <- function(x)
{
  len = length(x) - 1
  res = 0
  for (i in c(1:len))
  {
    f = 100*(x[i+1] - x[i]^2)^2 + (x[i] - 1)^2
    res = res + f
  }

  return (res)
}

easomFunction <- function(x)
{
  res = -cos(x[1]) * cos(x[2]) * exp(-((x[1]-pi)^2 + (x[2]-pi)^2))

  return (res)
}

holderTableFunction <- function(x)
{
  res = - abs( sin(x[1]) * cos(x[2]) * exp( abs( 1 - ( sqrt(x[1]^2 + x[2]^2) ) / pi)))

  return (res)
}


