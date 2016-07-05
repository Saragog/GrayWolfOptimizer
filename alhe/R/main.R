#Param A
#n - aktualna iteracja funkcji
paramA = function(dim,maxIt,n,D)
{
  r1 = runif(dim)
  a = 2*((maxIt - n) / maxIt)
  A = (2 * a * r1 - a) * D
  return(A)
}

#Param C
paramC = function(dim,E)
{
  r2 = runif(dim)
  C = 2 * r2 * E
  return(C)
}

#d
d = function(C, X, Xobecne)
{
  res = abs(C * X - Xobecne)

  return (res)
}


#X
x = function(X, A, D)
{
  X1 = X - A * D

  return (X1)
}


#X(t+1)
nextX = function(X1, X2, X3)
{
  res = (X1 + X2 + X3)/3

  return (res)
}


#ciecie do dziedziny
cutMinMax <- function(x, vecMin, vecMax)
{
  for (i in 1:length(x))
  {
    if (x[i] > vecMax[i])
    {
      x[i] = vecMax[i]
    }
    if (x[i] < vecMin[i])
    {
      x[i] = vecMin[i]
    }
  }
  return (x)
}

#losowanie wilkow
generateWolf <- function(vecMin, vecMax)
{

  randomValues = c()
  if (length(vecMin) == length(vecMax))
  {
    # bajlando


    rand = runif(length(vecMin))
    #randomValues = vector (length = length(vecMin))


    for (i in 1:length(vecMin))
    {
      dif = vecMax[i] - vecMin[i]

      val = rand[i] * dif + vecMin[i]

      randomValues = append(randomValues, val)
    }

  }

  return (randomValues)
}

# function for finding best wolf
findBestWolf = function(values)
{
  best = values[1]
  number = 1

  for (i in 2:length(values))
  {
    if (best > values[i])
    {
      best = values[i]
      number = i
    }
  }

  return (number)
}

grayWolfOptimizer = function(goal,dim,pars)
{
  # pars$D - D
  # pars$E - E
  # pars$maxIt - n
  # pars$vecMin - vector min dziedziny
  # pars$vecMax - vector max dziedziny
  # pars$wolfsNumber - wolvesNumber
  if (length(pars) < 6)
  {
    # komunikat zbyt malej ilosci argumentow w pars
    return(F)
  }

    D = pars$D
    E = pars$E
    maxIt = pars$maxIt
    vecMin = pars$vecMin
    vecMax = pars$vecMax
    wolvesNumber = pars$wolvesNumber


    # poczatkowa generacja wilkow

    wolvesOmega = matrix(ncol=2,nrow=0)



    for (i in 1:wolvesNumber)
    {
      tempWolf = generateWolf(vecMin, vecMax)
      # nastepnie trzeba wyliczac wartosci dla positionOfWolves

      tempWolfList = list(x=tempWolf,y=goal(tempWolf))

      wolvesOmega = rbind(wolvesOmega,tempWolfList)

    }

    # znalezienie 3 najlepszych wilkow (alfa beta delta)




    number = findBestWolf(unlist (wolvesOmega[,2]) )
    wolfAlpha = wolvesOmega[number, ]
    wolvesOmega = wolvesOmega[-number, ]

    number = findBestWolf(unlist (wolvesOmega[,2]) )
    wolfBeta = wolvesOmega[number, ]
    wolvesOmega = wolvesOmega[-number, ]

    number = findBestWolf(unlist (wolvesOmega[,2]) )
    wolfDelta = wolvesOmega[number, ]
    wolvesOmega = wolvesOmega[-number, ]

    # glowna petla programu


    for (i in 1:maxIt)
    {
        for (j in 1:(wolvesNumber- 3))
        {
          Dalpha = d(paramC(dim,E),wolfAlpha$x,wolvesOmega[j, ]$x)
          Dbeta = d(paramC(dim,E),wolfBeta$x,wolvesOmega[j, ]$x)
          Ddelta = d(paramC(dim,E),wolfDelta$x,wolvesOmega[j, ]$x)

          x1 = x(wolfAlpha$x,paramA(dim,maxIt,i,D), Dalpha)
          x2 = x(wolfBeta$x,paramA(dim,maxIt,i,D), Dbeta)
          x3 = x(wolfDelta$x,paramA(dim,maxIt,i,D), Ddelta)



          wolvesOmega[j, ]$x = (x1 + x2 + x3)/3

          # cutting to min / max of area
          wolvesOmega[j, ]$x = cutMinMax(wolvesOmega[j, ]$x, vecMin, vecMax)

          # liczenie nowej wartosci dla przemieszczonego wilka


          wolvesOmega[j, ]$y = goal(wolvesOmega[j, ]$x)

        }

        # zmiana hierarchi wilkow w stadzie

      for (i in 1:3)
      {


          best = findBestWolf(unlist (wolvesOmega[,2]))


          if (wolvesOmega[best,]$y < wolfDelta$y) # nastapi zmiana z delta
          {


            tempX = wolvesOmega[best,]$x
            tempY = wolvesOmega[best,]$y

            wolvesOmega[best,]$x = wolfDelta$x
            wolvesOmega[best,]$y = wolfDelta$y

            wolfDelta$x = tempX
            wolfDelta$y = tempY

            if (wolfDelta$y < wolfBeta$y)#zamiana z beta
            {


              tempX = wolfDelta$x
              tempY = wolfDelta$y

              wolfDelta$x = wolfBeta$x
              wolfDelta$y = wolfBeta$y

              wolfBeta$x = tempX
              wolfBeta$y = tempY

              if (wolfBeta$y < wolfAlpha$y)#zmiana z alpha
              {


                tempX = wolfBeta$x
                tempY = wolfBeta$y

                wolfBeta$x = wolfAlpha$x
                wolfBeta$y = wolfAlpha$y

                wolfAlpha$x = tempX
                wolfAlpha$y = tempY

              }
            }

          }else
          {
            break
          }
      }
      print(wolfAlpha$y)
    }


  return (wolfAlpha)
}
