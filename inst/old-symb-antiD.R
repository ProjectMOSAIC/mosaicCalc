##########  OLD Symbolic anti-derivative, pre-version 0.5.7


# #' Use recursion to find a symbolic antiderivative
# #'
# #' #(at)rdname symbolicInt
# #'
# #' @return a formula implementing giving symbolic anti-derivative.  If the formula
# #' isn't found by the algorithm, an error is thrown.
# #'
# #' #(at)export

symbolicAntiD <- function(form, ...){
  rhsVar = all.vars(rhs(form))
  if(length(rhsVar)!=1) stop("Can only integrate with respect to one variable.")
  constants = setdiff(all.vars(form), rhsVar)
  
  #check if it's just constants
  if(length(grep(rhsVar, deparse(lhs(form), width.cutoff=500)))==0){
    form[[2]]<- parse(text = paste(deparse(lhs(form), width.cutoff=500), "*", rhsVar, sep=""))[[1]]
    return(form)
  }
  
  #check to see if surrounded by parentheses
  if(class(lhs(form))=="("){
    form[[2]]=lhs(form)[[2]]
    return(symbolicAntiD(form, ...))
  }
  
  #Check to see if it is nested
  if(class(lhs(form))=="call"&&is.primitive(eval(lhs(form)[[1]])))
    group = getGroup(toString(lhs(form)[[1]]))[[1]] #determine typ of highest-level expr.
  else group = -1
  if(group =="Arith")
    return(.intArith(form, ...))
  if(group =="Math")
    return(.intMath(form, ...))
  
  #check if it's just x
  if((lhs(form))==rhsVar){
    form[[2]]<- parse(text=paste("1/(2)*", rhsVar, "^2", sep=""))[[1]]
    return(form)
  }
  
  stop("Error: symbolic algorithm gave up")
}

# #' Attempts symbolic integration of some mathematical/arithmetical forms
# #'
# #'
# #' @return An expression with the integral, or throws an error if unsuccessful.
# #'
.intArith <- function(form, ...){
  dots = list(...)
  
  rhsVar = all.vars(rhs(form))
  constants = setdiff(all.vars(form), rhsVar)
  
  op = lhs(form)[[1]]
  
  if(length(lhs(form))==2){#binary operation
    if(op=='-'){
      form[[2]] = parse(text=paste("(-1)*",deparse(lhs(form)[[2]], width.cutoff=500),sep=""))[[1]]
      return(symbolicAntiD(form,...))
    }
  }
  
  if(op =='+'||op =="-"){
    lform = form
    rform = form
    lform[[2]] = lhs(form)[[2]]
    rform[[2]] = lhs(form)[[3]]
    lform = symbolicAntiD(lform, ...)
    rform = symbolicAntiD(rform, ...)
    
    # newform = parse(text=paste(deparse(lhs(lform), width.cutoff=500),
    #                            deparse(lhs(form)[[1]], width.cutoff=500),
    #                            deparse(lhs(rform), width.cutoff=500), sep=""))[[1]]
    
    newform <- form[[2]]
    newform[[2]] <- lhs(lform)
    newform[[3]] <- lhs(rform)
    
    form[[2]] <- newform #simplify_mult(newform)
    return(form)
  }
  
  if(op == '*'){
    lform = form
    rform = form
    lform[[2]] = lhs(form)[[2]]
    rform[[2]] = lhs(form)[[3]]
    if(length(grep(rhsVar, deparse(lform[[2]], width.cutoff=500)))>0 &&
       length(grep(rhsVar, deparse(rform[[2]], width.cutoff=500)))>0)#too complex
      stop("Error: symbolic algorithm gave up")
    if(regexpr(rhsVar, deparse(lform[[2]], width.cutoff=500))==1){
      lform = symbolicAntiD(lform, ...)
      
      # newform = parse(text=paste(deparse(lhs(lform), width.cutoff=500),
      #                            deparse(lhs(form)[[1]], width.cutoff=500),
      #                            deparse(lhs(rform), width.cutoff=500), sep=""))[[1]]
      
      
      newform <- form[[2]]
      newform[[2]] <- lhs(lform)
      newform[[3]] <- lhs(rform)
      
      form[[2]] <- newform
      return(form)
    }
    else{
      rform = symbolicAntiD(rform, ...)
      
      # newform = parse(text=paste(deparse(lhs(lform), width.cutoff=500),
      #                            deparse(lhs(form)[[1]], width.cutoff=500),
      #                            deparse(lhs(rform), width.cutoff=500), sep=""))[[1]]
      
      
      newform <- form[[2]]
      newform[[2]] <- lhs(lform)
      newform[[3]] <- lhs(rform)
      
      form[[2]] <- newform #simplify_mult(newform)
      return(form)
    }
  }
  
  if(op=='/'){#let denominator have negative exponent if there is an x.
    num = lhs(form)[[2]]
    den = lhs(form)[[3]]
    
    #first see if it is a trigonometric substitution problem
    check <- try(.intTrig(form, num, den, rhsVar), silent=TRUE)
    if(!inherits(check, "try-error"))
      return(check)
    
    
    if(length(grep(rhsVar, den))>0){
      form[[2]] = parse(text = paste(deparse(num, width.cutoff=500), "*(",
                                     deparse(den, width.cutoff=500), ")^-1",sep="" ))[[1]]
      return(symbolicAntiD(form, ...))
    }
    else{
      form[[2]] = parse(text = paste("1/(",deparse(den, width.cutoff=500),")*",
                                     deparse(num, width.cutoff=500) , sep=""))[[1]]
      return(symbolicAntiD(form,...))
    }
  }
  
  if(op == '^'){
    
    affexp = .affine.exp(lhs(form)[[2]], rhsVar)
    if(length(affexp)>0 && length(grep(rhsVar, deparse(lhs(form)[[3]], width.cutoff=500)))==0){
      exp = try(eval(form[[2]][[3]], envir=list(pi=3.1415932653589793, form=form),
                     enclos=NULL), silent=TRUE)
      if(inherits(exp, "try-error"))
        exp = parse(text = paste(deparse(lhs(form)[[3]], width.cutoff=500), "+1"))[[1]]
      else(exp = eval(exp)+1) #change from call to numeric...
      
      if(exp == 0){
        
        if(affexp$a==1)
          form[[2]] = parse(text=paste("log(", deparse(lhs(form)[[2]], width.cutoff=500),
                                       ")", sep=""))[[1]]
        else
          form[[2]] <- parse(text = paste("1/(",deparse(affexp$a, width.cutoff=500) ,")*log(",
                                          deparse(lhs(form)[[2]]), ")", sep=""))[[1]]
        return(form)
      }
      form[[2]][[3]] <- exp
      if(affexp$a==1)
        newform <- paste("1/(", deparse(exp), ")*",
                         deparse(form[[2]]), sep="")
      else
        newform <- paste("1/(",deparse(affexp$a),")*1/(", deparse(exp), ")*",
                         deparse(form[[2]]), sep="")
      form[[2]] <- parse(text=newform)[[1]]
      return(form)
    }
  }
  
  stop("Error: symbolic algorithm gave up")
}

#--------------------------
# #' Attempts symbolic integration of some mathematical forms
# #'
# #'
# #' @return An expression with the integral, or throws an error if unsuccessful.
# #'
.intMath <- function(form, ...){
  
  op = lhs(form)[[1]]
  
  dots = list(...)
  
  rhsVar = all.vars(rhs(form))
  constants = setdiff(all.vars(form), rhsVar)
  
  if(op =="sin"){#trig expression
    #check to see if we can integrate it
    affexp = .affine.exp(lhs(form)[[2]], rhsVar)
    if(length(affexp)>0){
      if(affexp$a==1)
        newform = paste("-cos(", deparse(lhs(form)[[2]]), ")", sep="")
      else
        newform = paste("1/(", deparse(affexp$a), ")*-cos(", deparse(lhs(form)[[2]]),
                        ")", sep="")
      form[[2]]= parse(text=newform)[[1]]
      return(form)
    }
    else stop("Error: symbolic algorithm gave up")
  }
  
  if(op == "cos"){
    #check to see if we can integrate it
    affexp = .affine.exp(lhs(form)[[2]], rhsVar)
    if(length(affexp)>0){
      if(affexp$a==1)
        newform = paste("sin(", deparse(lhs(form)[[2]]), ")", sep="")
      else newform = paste("1/(", deparse(affexp$a), ")*sin(",
                           deparse(lhs(form)[[2]]), ")", sep="")
      form[[2]]= parse(text=newform)[[1]]
      return(form)
    }
    else stop("Error: symbolic algorithm gave up")
  }
  
  if(op == "exp"){
    #Check to see if we can integrate it
    affexp = .affine.exp(lhs(form)[[2]], rhsVar)
    if(length(affexp)>0){
      if(affexp$a==1)
        newform = paste("exp(", deparse(lhs(form)[[2]]), ")", sep="")
      else
        newform = paste("1/(", deparse(affexp$a), ")*exp(",
                        deparse(lhs(form)[[2]]), ")", sep="")
      form[[2]]= parse(text=newform)[[1]]
      return(form)
    }
    else stop("Error: symbolic algorithm gave up")
  }
  
  if(op == "tan"){
    #check to see if we can integrate it
    affexp = .affine.exp(lhs(form)[[2]], rhsVar)
    if(length(affexp)>0){
      if(affexp$a==1)
        newform = paste("-log(abs(cos(", deparse(lhs(form)[[2]]), ")))", sep="")
      else
        newform = paste("1/(", deparse(affexp$a), ")*-log(abs(cos(",
                        deparse(lhs(form)[[2]]), ")))", sep="")
      form[[2]]= parse(text=newform)[[1]]
      return(form)
    }
    else stop("Error: symbolic algorithm gave up")
  }
  
  if(op == "sinh"){
    #check to see if we can integrate it
    affexp = .affine.exp(lhs(form)[[2]], rhsVar)
    if(length(affexp)>0){
      if(affexp$a==1)
        newform = paste("cosh(", deparse(lhs(form)[[2]]), ")", sep="")
      else
        newform = paste("1/(", deparse(affexp$a), ")*cosh(",
                        deparse(lhs(form)[[2]]), ")", sep="")
      form[[2]]= parse(text=newform)[[1]]
      return(form)
    }
    else stop("Error: symbolic algorithm gave up")
  }
  
  if(op == "cosh"){
    #check to see if we can integrate it
    affexp = .affine.exp(lhs(form)[[2]], rhsVar)
    if(length(affexp)>0){
      if(affexp$a==1)
        newform = paste("sinh(", deparse(lhs(form)[[2]]), ")", sep="")
      else
        newform = paste("1/(", deparse(affexp$a), ")*sinh(",
                        deparse(lhs(form)[[2]]), ")", sep="")
      form[[2]]= parse(text=newform)[[1]]
      return(form)
    }
    else stop("Error: symbolic algorithm gave up")
  }
  
  if(op == "sinh"){
    #check to see if we can integrate it
    affexp = .affine.exp(lhs(form)[[2]], rhsVar)
    if(length(affexp)>0){
      if(affexp$a==1)
        newform = paste("log(cosh(", deparse(lhs(form)[[2]]), "))", sep="")
      else
        newform = paste("1/(", deparse(affexp$a), ")*log(cosh(",
                        deparse(lhs(form)[[2]]), "))", sep="")
      form[[2]]= parse(text=newform)[[1]]
      return(form)
    }
    else stop("Error: symbolic algorithm gave up")
  }
  
  if(op == "sqrt"){
    #check to see if we can integrate it
    affexp = .affine.exp(lhs(form)[[2]], rhsVar)
    if(length(affexp)>0){
      if(affexp$a==1)
        newform = paste("2/3*sqrt(", deparse(lhs(form)[[2]]), ")^3", sep="")
      else
        newform = paste("1/(", deparse(affexp$a), ")*2/3*sqrt(",
                        deparse(lhs(form)[[2]]), ")^3", sep="")
      form[[2]]= parse(text=newform)[[1]]
      return(form)
    }
    else stop("Error: symbolic algorithm gave up")
  }
  
  stop("Error: symbolic algorithm gave up")
}

#-------------------------

# #' Attempts symbolic integration of some mathematical forms using trigonometric substitution
# #'
# #'
# #' @param num numerator
# #' @param den denominator
# #' @param .x. the variable name

# #'
# #' @return An expression with the integral, or throws an error if unsuccessful.
# #'
.intTrig <- function(form, num, den, .x.){
  params <- all.vars(num)
  if(length(params) == 0)
    params <- ""
  numco <- .polyExp(num, .x., params)
  
  if(numco$pow != 0) stop("Not valid trig sub")
  
  if(den[[1]] == 'sqrt'){
    #Could be arcsin or arccos
    params <- setdiff(all.vars(den), .x.)
    if(length(params)==0)
      params <-""
    
    denco <- .polyExp(den[[2]], .x., params)
    if(denco$pow != 2) stop("Not valid trig sub")
    
    a <- denco$coeffs[[1]]
    b <- denco$coeffs[[2]]
    c <- denco$coeffs[[3]]
    
    if(sign(a)==-1){
      #arcsec?
      
    }
    #complete the square to go from form 1/ax^2+bx+c to 1/a(x-h)^2+k
    if(b==0){
      h <- 0
      k <- c
    }
    else{
      h <- parse(text = paste("-(", deparse(b), ")/(2*(", deparse(a), "))", sep=""))[[1]]
      k <- parse(text = paste(deparse(c), "-((", deparse(b), ")^2/(4*(", deparse(a), ")))", sep=""))[[1]]
    }
    if(is.numeric(c(a,b,c))){
      h <- eval(h)
      k <- eval(k)
    }
    
    #Check what the sign on a and k are.
    if(!is.numeric(a))
      asign=1
    else
      asign=sign(a)
    if(!is.numeric(k))
      ksign=1
    else
      ksign=sign(k)
    
    if(asign==-1&&ksign==1){
      #Arcsin
      if(a!=-1){
        #k <- parse(text = paste("(", deparse(k), ")/(-1*(", deparse(a), "))", sep=""))[[1]]
        num <- parse(text = paste("(", deparse(num), ")/sqrt(-1*(", deparse(a), "))", sep=""))[[1]]
      }
      k <- parse(text = paste("sqrt(", deparse(k), ")", sep=""))[[1]]
      #Now need to integrate it
      
      if(a==-1)
        expr <- parse(text = paste(deparse(num), "*asin((", .x., "-", deparse(h), ")/", deparse(k), ")", sep=""))[[1]]
      else
        expr <- parse(text = paste(deparse(num), "*asin((sqrt(-1*(", deparse(a), "))*(", .x., "-", deparse(h), "))/",
                                   deparse(k), ")", sep=""))[[1]]
      form[[2]] <- expr
      return(form)
      
    }
    
    if(asign==1&&ksign==1){
      #Arcsinh
      if(a!=1){
        #k <- parse(text = paste("(", deparse(k), ")/(-1*(", deparse(a), "))", sep=""))[[1]]
        num <- parse(text = paste("(", deparse(num), ")/sqrt(", deparse(a), ")", sep=""))[[1]]
      }
      k <- parse(text = paste("sqrt(", deparse(k), ")", sep=""))[[1]]
      #Now need to integrate it
      
      if(a==1)
        expr <- parse(text = paste(deparse(num), "*asinh((", .x., "-", deparse(h), ")/", deparse(k), ")", sep=""))[[1]]
      else
        expr <- parse(text = paste(deparse(num), "*asinh((sqrt(", deparse(a), ")*(", .x., "-", deparse(h), "))/",
                                   deparse(k), ")", sep=""))[[1]]
      form[[2]] <- expr
      return(form)
    }
    
    if(asign==1&&ksign==-1){
      #natural log problem
      #        if(a!=1){
      #          num <- parse(text = paste("(", deparse(num), ")/sqrt(", deparse(a), ")", sep=""))[[1]]
      #        }
      #       if(is.numeric(k))
      #         k <- k*-1
      #       else{
      #         if(is.call(k)&&k[[1]]=='-')
      #          k <- k[[2]]
      #         else
      #           k <- parse(text=paste("-1*(", deparse(k), ")", sep=""))[[1]]
      #       }
      #
      #        k <- parse(text = paste("(", deparse(k), ")", sep=""))[[1]]
      # #       #Now need to integrate it
      # #
      #        if(a==1){
      #          expr <- parse(text = paste(deparse(num), "*log(", .x., "-", deparse(h), "+
      #                sqrt((", .x., "-", deparse(h), ")^2-", deparse(k), "))", sep=""))[[1]]
      #         }
      #        else{
      #          expr <- parse(text = paste(deparse(num), "*log(", .x., "-", deparse(h), "+
      #                sqrt((", .x., "-", deparse(h), ")^2-", deparse(k), "/(", deparse(a), ")))", sep=""))[[1]]
      #          }
      #        form[[2]] <- expr
      #        return(form)
    }
    
  }
  else{
    params <- setdiff(all.vars(den), .x.)
    if(length(params)==0){
      params <- ""
    }
    denco <- .polyExp(den[[2]], .x., params)
    if(denco$pow != 2) stop("Not valid trig sub")
    
    a <- denco$coeffs[[1]]
    b <- denco$coeffs[[2]]
    c <- denco$coeffs[[3]]
    
    #complete the square to go from form 1/ax^2+bx+c to 1/a(x-h)^2+k
    if(b==0){
      h <- 0
      k <- c
    }
    else{
      h <- parse(text = paste("-(", deparse(b), ")/(2*(", deparse(a), "))", sep=""))[[1]]
      k <- parse(text = paste(deparse(c), "-((", deparse(b), ")^2/(4*(", deparse(a), ")))", sep=""))[[1]]
    }
    if(is.numeric(c(a,b,c))){
      h <- eval(h)
      k <- eval(k)
    }
    
    if(!is.numeric(a))
      asign=1
    else
      asign=sign(a)
    if(!is.numeric(k))
      ksign=1
    else
      ksign=sign(k)
    
    if(asign==ksign){
      #arctan
      if(a!=1){
        num <- parse(text = paste("(", deparse(num), ")/(", deparse(a), ")", sep=""))[[1]]
      }
      
      if(sign(a)==-1){
        num <- parse(text=paste("-1*(", deparse(num), ")", sep=""))[[1]]
        a <- parse(text = paste("-(", deparse(a), ")", sep=""))[[1]]
        k <- parse(text = paste("-(", deparse(k), ")", sep=""))[[1]]
        
      }
      
      if(a==1){
        expr <- parse(text = paste(deparse(num), "*sqrt(1/(", deparse(k), "))*atan((", .x., "-", deparse(h),
                                   ")/sqrt(", deparse(k), "))" , sep=""))[[1]]
      }
      else{
        expr <- parse(text = paste(deparse(num), "*sqrt((", deparse(a), ")/(", deparse(k), "))*atan(sqrt(", deparse(a),
                                   ")*(", .x., "-", deparse(h), ")/sqrt(", deparse(k), "))" , sep=""))[[1]]
      }
      
      form[[2]] <- expr
      return(form)
    }
    
  }
  
  stop("Not valid trig sub")
}


# #' Takes a call and returns its affine coefficients.
# #'
# #' #(at)rdname symbolicInt
# #'
# #' @param tree the expression to be analyzed
# #' @return A list with values of a and b satisfying a*.x.+b  = tree.
# #' If the expression is not affine, returns an empty list.
# #'
.affine.exp <- function(tree, .x.){
  #if it is a simple expression
  if(tree==.x.){
    a=1
    b=0
    return(list(a=a,b=b))
  }
  
  #if there is no variable in the expression
  if(length(grep(toString(.x.), deparse(tree), fixed=TRUE))==0){
    a=0
    b=tree
    return(list(a=a,b=b))
  }
  
  #if the expression is more complex
  if(tree[[1]]=='('){
    return(Recall(tree[[2]], .x.))
  }
  
  if(tree[[1]]=='+'){
    lexp=Recall(tree[[2]], .x.)
    rexp=Recall(tree[[3]], .x.)
    
    if(length(lexp)==0||length(rexp)==0)
      return(list())
    
    if(lexp$a==0 && length(rexp)!=0){
      a = rexp$a
      b = parse(text=paste(deparse(lexp$b), "+", deparse(rexp$b),sep=""))[[1]]
      return(list(a=a,b=b))
    }
    
    if(rexp$a==0 && length(lexp)!=0){
      a = lexp$a
      b = parse(text=paste(deparse(lexp$b), "+", deparse(rexp$b),sep=""))[[1]]
      return(list(a=a,b=b))
    }
  }
  
  if(tree[[1]]=='-'){
    lexp=Recall(tree[[2]], .x.)
    rexp=Recall(tree[[3]], .x.)
    
    if(length(lexp)==0||length(rexp)==0)
      return(list())
    
    if(lexp$a==0 && length(rexp)!=0){
      a = parse(text=paste("-1*(",deparse(lexp$a), ")",sep=""))[[1]]
      b = parse(text=paste(deparse(lexp$b), "-(", deparse(rexp$b),")",sep=""))[[1]]
      return(list(a=a,b=b))
    }
    
    if(rexp$a==0 && length(lexp)!=0){
      a = lexp$a
      b = parse(text=paste(deparse(lexp$b), "-(", deparse(rexp$b),")",sep=""))[[1]]
      return(list(a=a,b=b))
    }
  }
  
  if(tree[[1]]=='*'){
    lexp=Recall(tree[[2]], .x.)
    rexp=Recall(tree[[3]], .x.)
    
    if(length(lexp)==0||length(rexp)==0)
      return(list())
    
    if(lexp$a==0 && length(rexp)!=0){
      a = parse(text=paste("(",deparse(lexp$b), ")*(", deparse(rexp$a),")",sep=""))[[1]]
      b = parse(text=paste("(",deparse(lexp$b), ")*(", deparse(rexp$b),")",sep=""))[[1]]
      if(lexp$b==0){
        a = 0
        b = 0
      }
      if(lexp$b==1){
        a = rexp$a
        b = rexp$b
      }
      if(rexp$a ==0){
        a = 0
      }
      if(rexp$b == 0){
        b = 0
      }
      if(rexp$a == 1){
        a = lexp$b
      }
      if(rexp$b == 1){
        b = lexp$b
      }
      return(list(a=a,b=b))
    }
    
    if(rexp$a==0 && length(lexp)!=0){
      a = parse(text=paste("(",deparse(lexp$a), ")*(", deparse(rexp$b),")",sep=""))[[1]]
      b = parse(text=paste("(",deparse(lexp$b), ")*(", deparse(rexp$b),")",sep=""))[[1]]
      
      if(rexp$b==0){
        a = 0
        b = 0
      }
      if(rexp$b==1){
        a = lexp$a
        b = lexp$b
      }
      if(lexp$a==0){
        a = 0
      }
      if(lexp$b==0){
        b = 0
      }
      if(lexp$a==1){
        a = rexp$b
      }
      if(lexp$b==1){
        b = rexp$b
      }
      return(list(a=a,b=b))
    }
  }
  
  if(tree[[1]]=='/'){
    lexp=Recall(tree[[2]], .x.)
    rexp=Recall(tree[[3]], .x.)
    
    if(length(lexp)==0||length(rexp)==0)
      return(list())
    
    if(rexp$a==0 && length(lexp)!=0){
      a = parse(text=paste(deparse(lexp$a), "/(", deparse(rexp$b),")",sep=""))[[1]]
      b = parse(text=paste(deparse(lexp$b), "/(", deparse(rexp$b),")",sep=""))[[1]]
      
      if(rexp$b==1){
        a = lexp$a
        b = lexp$b
      }
      
      return(list(a = a, b = b))
    }
  }
  return(list())
}

# #' Simplifying expressions, e.g. pure numbers go to numbers
# #' Written by Aaron Mayerson, May 2013
# #' #(at)rdname mosaic-internal
OLD.makeNice <- function(form,params=all.vars(form)){
  # See if the MASS package fraction simplifier is installed, if not
  # leave fractions in decimal form
  # Belay that order.  We now require MASS, so it will be there.
  # So next line is now commented out.
  # # if (!require(MASS,quietly=TRUE)) fractions <- I
  
  # Functions where the contents are preserved, e.g. sqrt(2)
  forbidden = c("exp","sin","cos","tan","acos", "asin",
                "atan","log","sqrt","log10","tanh","atanh",
                "cosh","acosh","sinh","asinh")
  # Can we get a number from it?
  val <- try(eval(form),silent=TRUE)
  if(is.numeric(val)){
    vars <- all.vars(form)
    if(length(vars) > 0){
      if(prod(vars %in% params) == 0){
        names <- all.names(form)
        if(sum(names %in% forbidden) == 0){
          if(!(charToRaw("\136") %in% charToRaw(toString(form)))){
            val <- parse(text=paste("",fractions(val),""))[[1]]
            return(val)
          }
        }
      }
    }
    else{
      names <- all.names(form)
      if(sum(names %in% forbidden) == 0){
        if(!(charToRaw("\136") %in% charToRaw(toString(form)))){
          val <- parse(text=paste("",fractions(val),""))[[1]]
          return(val)
        }
      }
    }
  }
  if(length(form) == 1){
    return(form)
  }
  else{
    for(i in 1:length(form)){
      form[[i]] <- .makeNice(form[[i]],params)
    }
    return(form)
  }
}

# Old numerical antiD

# ===================
# The function returned by antiD will take the same arguments as
# f, but will split one of the variables into a "to" and "from" component.
# The "from" will have a default value of 0 or otherwise inherited from
# the call to antiD
# The variable of integration will be called "viName"
#' @rdname Calculus
#'
#' @param .function function to be integrated
#' @param .wrt character string naming the variable of integration
#' @param from default value for the lower bound of the integral region
# I don't want this function to be exported.
makeAntiDfun <- function(.function, .wrt, from, .tol=.Machine$double.eps^0.25) {
  resargs <- formals(.function)
  
  intC <- LETTERS[-(1:2)][!LETTERS[-(1:2)]%in% names(resargs)][1]
  if (length(intC)==0) intC <- paste("ConstantOfIntegration",runif(1),sep="")
  resargs[intC] <- 0
  
  # Create a new function of argument .vi that will take additional
  # arguments
  .newf <- function(.vi,.av){
    .av[[.wrt]] = .vi
    do.call(.function,.av,quote=TRUE) + 0*.vi  # make the same size as vi
  }
  # Create the numerical integral
  res <- function(){
    numerical_integration(.newf,.wrt,as.list(match.call())[-1],formals(),
                          from,ciName=intC, .tol)
  }
  
  formals(res) <- c(resargs)
  ## Vectorize at the end
  # return(Vectorize(res))
  return(res)
}
# =============
#' @rdname Calculus
#'
#' @param f A function.
#' @param wrt Character string naming a variable: the var. of integration.
#' @param av A list of the arguments passed to the function calling this.
#' @param args Default values (if any) for parameters.
#' @param vi.from The the lower bound of the interval of integration.
#' @param ciName Character string giving the name of the symbol for the constant of integration.
#' @param .tol Numerical tolerance.  See \code{\link[stats]{integrate}()}.
#'
#' @note \code{numerical_integration} is not intended for direct use.  It packages
#' up the numerical anti-differentiation process so that the contents
#' of functions produced by \code{antiD} look nicer to human readers.
#' @export
#'
numerical_integration <- function(f,wrt,av,args,vi.from, ciName="C",.tol) {
  # We are about to do the numerics.  At this point, every
  # variable should have a numerical binding.  Just in case some
  # are still expressions, go through the list and evaluate them
  for(k in 1:length(av)) av[[k]] = eval.parent(av[[k]],n=2)
  av2 = c(av, args) # combine the actual arguments with the formals
  # to make sure that default values are included
  # Extract the limits from the argument list
  vi.to <- inferArgs(wrt, av2, defaults=alist(val=NaN),
                     variants = c("","to",".to"))$val
  # If they are calls, turn them into values.  Redundant with loop above
  if( any(is.nan(vi.to)) | any(is.nan(vi.from))) stop("Integration bounds not given.")
  # and delete them from the call
  av[[paste(wrt,".from",sep="")]] <- NULL
  av[[paste(wrt,".to",sep="")]] <- NULL
  initVal <- av2[[ciName]]
  av[[ciName]] <- NULL
  newf <- function(vi){
    av[[wrt]] = vi
    # make the same size as input vi
    f(vi,av) + 0*vi
  }
  # NEED TO ADD TOLERANCE
  
  multiplier <- 1
  if( length(vi.from) > 1 & length(vi.to) == 1 ){
    temp <- to
    to <- from
    from <- temp
    multiplier <- -1
  }
  # handle situation where both from and to are a set of values.
  if( length(vi.from)>1 & length(vi.to)>1 ){
    if( length(vi.from)!=length(vi.to) ) stop("Either fix 'from' or set it to the same length as 'to'")
    res <- rep(0,length(vi.to))
    for (k in 1:length(vi.to)) {
      res[k] <- stats::integrate(newf,vi.from[k],vi.to[k],rel.tol=.tol)$value
    }
    return(res+initVal)
  }
  val0 <- stats::integrate(newf, vi.from, vi.to[1],rel.tol=.tol)$value
  # work around a bug in integrate()
  if( vi.to[1]==-Inf ) { # Aaron Meyerson fix June 2013
    # A couple of bugs have popped up when upper limit is -Inf,
    # so upper and lower are switched and we take the negative
    val0 <- -1*stats::integrate(newf, vi.to[1], vi.from, rel.tol=.tol)$value
  }
  # add initial condition
  val0 <- val0 + initVal
  if (length(vi.to) == 1) {
    return(multiplier*val0)
  }
  res <- rep(val0, length(vi.to))
  for (k in 2:length(res)) {
    res[k] <- stats::integrate(newf, vi.to[k-1], vi.to[k],rel.tol=.tol)$value
  }
  res <- cumsum(res)
  return(multiplier*res)
}
