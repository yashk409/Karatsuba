fun explodedIntList str  = let val charList = (explode(str))
							in map(fn ch => (Char.ord(ch) - 48)) charList
							end
fun intListtoString L = let val charList = map(fn ch => (Char.chr(ch+48))) L
                        in implode(charList)
                        end

fun correctList (L1,L2) = if length L1<length L2 then 
                                correctList(0::L1,L2) 
                                else if  length L1>length L2 then correctList(L1,0::L2) 
                                else (L1,L2)
fun padZeroesBack (L,0) = L
    | padZeroesBack (L,m) = padZeroesBack((L@[0]),m-1)

fun padZeroesAhead (L,0) = L
    | padZeroesAhead (L,m) = padZeroesAhead(0::L,m-1)

fun split L = let val size = List.length L
                    
                    val M = List.take(L,(size div 2))
                    val N = List.drop(L,(size div 2))
                    val (m,n) = correctList(M,N)
                in (m,n)
                end

fun addList ([],[],carry) = if carry=0 then [] else [carry]
    | addList ((h1::t1),(h2::t2),carry) = let val sum = (h1+h2+carry) mod 10;
                                             val car = (h1+h2+carry) div 10;    
                                         in sum::addList(t1,t2,car)
                                         end
                        
fun finalAdd (L1,L2) =
            let 
                val (L3,L4)=correctList(L1,L2)
            in
             List.rev(addList(List.rev(L3),List.rev(L4),0))
             end

fun fix [a]= [a]
    |fix (L1 as h::t) =
        if h=0 then fix (t) else L1

fun findGreater ([],[]) = true
    |findGreater ((h1::t1),(h2::t2)) = if h1=h2 then findGreater(t1,t2)
                                      else if h1>h2 then true
                                      else false  
         
fun subtractList (L1,L2) = if(findGreater(L1,L2)) then ListPair.map(fn(i,j) => (i-j)) (L1,L2)
                           else ListPair.map(fn(i,j) => (i-j)) (L2,L1)


fun correctListSubtraction [a] = [a]
    |correctListSubtraction (h::t) =  let val newh = if(h<0) then h+10 else h
                                          val newt = if(h<0) then List.update(t,0,List.nth(t,0)-1) else t
                                    
                                        in newh::correctListSubtraction(newt)
                                        end
fun finalSubtract (L1,L2) = 
                    let val(L3,L4)=correctList(L1,L2);
                    in
                    List.rev(correctListSubtraction(List.rev(subtractList(L3,L4))))
                    end
                                       
fun karatsuba ([a],[b]) = let val mult = a*b mod 10
                            val carry = a*b div 10
                        in [carry,mult]
                        end            
    | karatsuba (L1 as (h1::t1),L2 as (h2::t2)) = let 
                                                    val m=(List.length(L1)+1) div 2;
                                                    val (x1,x0) = split L1;
                                                    val (y1,y0) = split L2;
                                                    val z2 = fix(karatsuba(x1,y1));
                                                    val z0 = fix(karatsuba(x0,y0));
                                                    val z2andz0 = fix(finalAdd(z2,z0));
                                                    val x1andx0 = fix(finalAdd(x1,x0));
                                                    val y1andy0 = fix(finalAdd(y1,y0));
                                                    val temp = fix(karatsuba(x1andx0,y1andy0));
                                                    val z1 = fix(finalSubtract(temp,z2andz0));
                                                    val addz2 = padZeroesBack(z2,(2*m));
                                                    val numz0 = List.length(addz2)-List.length(z0);
                                                    val addz0 = padZeroesAhead(z0,numz0);
                                                    val tempz1 = padZeroesBack(z1,m);
                                                    val numz1 = List.length(addz2) - List.length(tempz1);(*here was the error, error fixed bitch!!!*)
                                                    val addz1 = padZeroesAhead(tempz1,numz1);
                                                    val z1z0 = finalAdd(addz0,addz1);
                                                    val z2z1z0 = finalAdd(z1z0,addz2);
                                                    in
                                                        z2z1z0
                                                    end
    
fun finalKaratsuba (str1,str2) =intListtoString(karatsuba(explodedIntList(str1),explodedIntList(str2)))