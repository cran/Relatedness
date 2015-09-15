.AddDiag <-
function(mat,Vec){
  return(mat+diag(Vec))
}
.ClassIBSUnphased <-
function(ObsSnp){
  Obs <- paste(as.character(ObsSnp[1]),as.character(ObsSnp[2]))
  
  res <- switch(Obs,
                "0 0" = 0,
                "0 1" = 1,
                "0 2" = 2,
                "1 0" = 3,
                "1 1" = 4,
                "1 2" = 5,
                "2 0" = 6,
                "2 1" = 7,
                "2 2" = 8)
  return(res)
}
.EM <-
function(ProbCond , DeltaInit , Proj, Prec){
ProbCondVec <- c(t(ProbCond))
ProjVec <- c(t(Proj))
.C("BoucleEMacc" , as.double(ProbCondVec) , as.integer(dim(ProbCond)[2]) , as.integer(dim(ProbCond)[1]) , as.double(Prec) , as.double(ProjVec) , as.double(DeltaInit) , as.double(rep(0,dim(ProbCond)[1]*dim(ProbCond)[2])))
}
.LogVrai <-
function(CondVrais){
  res <- sum(log(rowSums(CondVrais)))
  return(res)
}
.MatTriSup <-
function(mat,Vecteur){
  mat[lower.tri(mat,diag=F)] <- Vecteur
  return(t(mat))
}
.Random.seed <-
c(403L, 1L, -505046913L, -961643563L, -482994708L, 404150082L, 
2047514781L, 326836503L, 1432172670L, -652865480L, 750487323L, 
1748206713L, 198708424L, 196467958L, 23534897L, 1178074467L, 
1489919794L, -105703292L, 233260231L, -1592252515L, -838962412L, 
-1949146694L, -468944891L, 880807391L, 965886054L, 1758097392L, 
-1173305069L, -414746959L, 659210592L, -1179584770L, -1266265463L, 
-1263080005L, 1894201226L, 379698796L, 1040048111L, 113768485L, 
-1739384612L, 310492850L, -2133413459L, -952535705L, 769049230L, 
-357069976L, -245763221L, -1986114295L, -1952480360L, 1587221190L, 
-1916266463L, -1577276237L, 515026466L, -1457311724L, 55407767L, 
-591651187L, 462507940L, 1160417674L, -1377186315L, -787275697L, 
1618564950L, -1014542848L, -1124139197L, -1383262495L, 620672080L, 
1915305134L, -1702836135L, -1337956245L, 1688727002L, -1416636772L, 
123250847L, -1064656139L, 466182028L, -1397649822L, 1744745021L, 
-1534296905L, -1217395874L, -931256680L, -1474935301L, 1622398873L, 
255025192L, 754985174L, -1274335919L, -1203897341L, 1074717586L, 
1389646116L, 616843239L, 1406188605L, 1765862068L, -1748580070L, 
575228517L, -2023932161L, 338084742L, -960969456L, 459185587L, 
750222929L, -1036255744L, -89826594L, 305297705L, 162579675L, 
2005976042L, -750226548L, -57707313L, 80600197L, -1436592324L, 
2073708050L, -689912563L, 2049224263L, 1446589486L, 543766920L, 
-1258913653L, -2066818455L, 64480824L, -130752794L, -1609991551L, 
473720339L, -1013534270L, 1276869236L, 879107191L, -1841339027L, 
-1800078844L, 1968806186L, 1984261781L, 155108655L, 558891062L, 
-2081622944L, -959153501L, 1343802817L, 510077360L, -1523615538L, 
930285113L, 1013160011L, -1088976390L, -689158916L, -69539521L, 
-1180539371L, 2082960684L, -688867838L, -283489059L, 136230871L, 
-2032041282L, -702950152L, 1331538651L, -220532423L, 125236360L, 
-587762634L, 602886257L, 513442467L, -768059406L, 50559812L, 
1103287303L, -1216663075L, -1354797356L, 828477946L, 1800437061L, 
497633695L, 1200110374L, 2025006000L, 1018540115L, 2080385905L, 
-540747104L, -120623554L, -801553079L, -2088485893L, 496582602L, 
-559541972L, 1889340719L, 1253012453L, -1899681124L, -408441102L, 
-427303059L, -2048198489L, -119645490L, 1659455528L, -1666362325L, 
-1468524983L, 1509760856L, -594109946L, 611009505L, 860299635L, 
-2142006814L, -96639660L, -1814389673L, -1607327667L, -1034083356L, 
1992851914L, 760547381L, -1910307185L, -1326598250L, 191224640L, 
1418384899L, -152654815L, 1985281808L, 1897436398L, 1052081305L, 
381147435L, -1832841190L, 632244956L, -1791108385L, -946920267L, 
-1635888564L, 127262370L, 1125705213L, 1894223607L, 2007099422L, 
433015512L, -1150108101L, -508400679L, 1744779624L, 484757654L, 
-1789907439L, -1341470781L, -1686127662L, 1747256932L, -1403841113L, 
1345620733L, 1159220980L, -1013388838L, -763155163L, -668861377L, 
1751952326L, -1138620336L, 310666355L, -1972696943L, -713074496L, 
-1065233762L, -698642839L, -1026777445L, 1588884394L, 616961996L, 
-1740019057L, -620821819L, 512092796L, 385787528L, -190374086L, 
178934608L, 909708396L, -1499205228L, 133887058L, -716914208L, 
-1513657140L, -507624288L, 1496679810L, -2093684504L, 178083564L, 
-553892548L, 908156018L, -2003093392L, -502976476L, -1922999240L, 
-1124143334L, 645049552L, -1759592644L, 1759786900L, 1173480322L, 
-1356240448L, 601685564L, 996042128L, -55140830L, -1403271928L, 
-1194688884L, 502394652L, -503185262L, -436569728L, 1556272660L, 
-1567248600L, 1133799994L, 355286480L, 753174892L, 1260127156L, 
-444614638L, -330824224L, -420161588L, 840483040L, -1035749342L, 
701280552L, 1858965004L, 752576316L, -2039999502L, -1196528912L, 
-395301308L, -64187816L, -311075014L, -626059792L, 1564800700L, 
641538324L, -20786494L, -544439200L, -1184754820L, 152135632L, 
-1208045278L, -938263896L, 952859532L, 2021247164L, -2030567630L, 
-2036078016L, -833807180L, -180882872L, -206834246L, -122617328L, 
-1727241492L, -1538154156L, -607608558L, 695430560L, -728485172L, 
802192992L, 1393514434L, -1763068120L, 312684460L, 694924092L, 
-129824398L, -1802233040L, 1184611620L, -2093544648L, 1682304730L, 
-621262960L, -380662020L, -281363564L, -1385418686L, -655682304L, 
-584596420L, -254213808L, 1117159586L, 1898787144L, -52754676L, 
-251919652L, 136093010L, -1618946944L, -340758444L, 1596116968L, 
661874938L, 1879579472L, 738180908L, 124176756L, 1823255442L, 
-1237518240L, 1004051852L, 2070330208L, -1499235870L, -595962968L, 
-1064495924L, 523554812L, 324871602L, 510481904L, 721677252L, 
-223595176L, -123102854L, 208683952L, 588257724L, -72071084L, 
128288706L, -1342238688L, -691828548L, 48685648L, 1406149602L, 
-1387339672L, -1915374516L, -138286852L, 326937586L, 1665853696L, 
-1484566156L, 1833335816L, -589967430L, -1691352624L, -1233523732L, 
329363860L, 285405650L, 249454944L, -1113596084L, -487155424L, 
-983060606L, -1051648280L, -1481535252L, -858147780L, 811412338L, 
140738160L, 18510884L, -604812488L, -1688138598L, 357659344L, 
1304068284L, -1630818668L, 213239426L, -390031168L, 2100425532L, 
-78046064L, 1379163682L, 428228616L, -2050163444L, 1421450396L, 
-1088938478L, -663634048L, -1405751788L, -1440445912L, 641156410L, 
1883363792L, -1443646356L, 652097844L, -1142462830L, 1643350880L, 
1203372876L, 1817090016L, -1861245022L, -1975990360L, -530526836L, 
-702289732L, 1283000690L, 764687600L, -1719664572L, 722047320L, 
-1554434502L, 1477024624L, 329349180L, -1512497388L, -438045246L, 
550484704L, 1273046908L, -368349488L, -791267166L, -811017176L, 
1381770508L, -883873860L, 991488306L, 1629120960L, 1887275572L, 
575482824L, -1797031750L, 1844943248L, 275247596L, -223051180L, 
1922007186L, -1428488416L, 30026188L, 118904288L, -1294272830L, 
-919821272L, 1435562668L, 1437516988L, -681936398L, 1375125808L, 
1750373540L, 125691576L, 1306594650L, -1617369712L, -625290372L, 
327350548L, -2034920638L, -2023245056L, -1409026116L, -1364705456L, 
-2132655326L, 2014010696L, -756286964L, 1156634716L, -1993814830L, 
-1208985088L, 240209236L, 606378472L, -1061217286L, -665440048L, 
124861814L, 1531346819L, 1671284165L, -980444446L, -1372871856L, 
-1966047623L, -1038556757L, 1048278812L, -472776630L, -182442945L, 
-1866496887L, -1273868130L, -597828276L, -943903459L, -2042713945L, 
147548112L, -95514770L, 397133339L, 1238478717L, 2121315690L, 
1554115592L, -548760399L, -819727581L, -1209352156L, 1816236754L, 
2128049959L, -1969469071L, 497315766L, 733750100L, 1090424293L, 
-1029367825L, -876841176L, -942813306L, -1614767181L, 782568213L, 
-1202920078L, 986978752L, 1568545321L, -782877029L, 172590956L, 
1838317434L, 372983983L, 1605796793L, -1321605682L, 1717702940L, 
683906189L, -141993737L, 546108416L, -929933730L, 1153476907L, 
-262686163L, -1111733606L, 1155634776L, 184285633L, -1911814541L, 
754746260L, 486767138L, 1963935031L, -1678253055L, -1912723194L, 
203152708L, 475947125L, -2118798881L, 1013505336L, -1504735466L, 
1503358755L, -254481179L, 364058882L, -522118288L, 937227033L, 
185172747L, 78008316L, 28044330L, -609484705L, -1639929879L, 
-564745986L, 1179556076L, 780540989L, -1439535097L, -696275984L, 
-1614018674L, -64408133L, -128371811L, 1383901514L, -1461149336L, 
2113857361L, -1218702269L, 1173364036L, -248144398L, -1332635705L, 
535132561L, 1329458454L, -1497614988L, -1694747643L, -198167473L, 
1785022536L, 758726118L, -1010674925L, 1423221877L, -882246062L, 
793311392L, -1023561335L, -1123892805L, -2125505012L, 697042842L, 
1898434319L, -723626663L, 522285038L, 593786492L, -708469011L, 
2080532887L, -947862944L, -474179522L, 1361977611L, 584651661L, 
-1124208454L, -1096403208L, -532210527L, -1635448749L, 633164660L, 
767398146L, -1901063785L, -172807327L, 625545126L, -191506012L, 
-399450027L, 238032127L, 384416152L, 1076413110L, 1170283331L, 
-255285627L, 310577954L, 1787637776L, -995021895L, 1877638251L, 
2108211036L, -462248310L, 849576575L, -1146745911L, 666286174L, 
-1200510196L, -1171000611L, 1840176103L, -994082800L, -740758226L, 
705733083L, 2018326205L, -2026150870L, -1404955448L, -459173391L, 
-1688903837L, -1785829532L, -1168916846L, -897369241L, 597108017L, 
1536081526L, 1303617428L, 874740645L, -760148817L, 766148968L, 
-1380423354L, -861716237L, 1317305941L, 1169613362L, 743018496L, 
986838249L, 366678875L, -1295690324L, 1958383034L, 955636546L
)
.RelatednessCouple <-
function(HybridGenom,Freq,Crossing,LinePop,Proj,Phased=FALSE,NbInit=5,Prec=10^(-4)){
NbSnp <- dim(HybridGenom)[1]
LinePopChar <- paste((1:4)[factor(LinePop,levels=unique(LinePop))],collapse=" ")
VecCrossChar <- paste((1:4)[factor(c(Crossing),levels=unique(c(Crossing)))],collapse=" ")


  
FreqPop <- Freq[,LinePop]
if (Phased==TRUE){
ImpossibleRelatednessCrossing <- switch(VecCrossChar,
                                          "1 1 1 1" = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14),
                                          "1 1 1 2" = c(1,2,3,4,5,6,7,8,9,10,12,13,14),
                                          "1 1 2 1" = c(1,2,3,4,5,6,7,8,9,10,11,13,14),
                                          "1 1 2 2" = c(1,2,3,4,5,6,7,9,10,11,12,13,14),
                                          "1 1 2 3" = c(1,3,4,5,6,7,9,10,13,14),
                                          "1 2 1 1" = c(1,2,3,4,5,6,7,8,9,10,11,12,14),
                                          "1 2 1 2" = c(1,2,3,4,5,6,7,8,10,11,12,13,14),
                                          "1 2 1 3" = c(1,2,3,5,6,7,8,10,12,14),
                                          "1 2 2 1" = c(1,2,3,4,5,6,7,8,9,11,12,13,14),
                                          "1 2 2 2" = c(1,2,3,4,5,6,7,8,9,10,11,12,13),
                                          "1 2 2 3" = c(1,2,3,4,5,7,8,9,12,13),
                                          "1 2 3 1" = c(1,2,3,4,6,7,8,9,11,14),
                                          "1 2 3 2" = c(1,2,3,4,5,6,8,9,12,13),
                                          "1 2 3 3" = c(1,2,4,5,6,7,9,10,11,12),
                                          "1 2 3 4" = c())
  
ImpossibleRelatednessPop <- switch(LinePopChar,
                                     "1 1 1 1" = c(),
                                     "1 1 1 2" = c(3,5,7,8,9,10,12,13,14,15),
                                     "1 1 2 1" = c(3,4,6,8,9,10,11,13,14,15),
                                     "1 1 2 2" = c(4,5,6,7,9,10,11,12,13,14,15),
                                     "1 1 2 3" = c(3,4,5,6,7,8,9,10,11,12,13,14,15),
                                     "1 2 1 1" = c(2,6,7,8,9,10,11,12,14,15),
                                     "1 2 1 2" = c(2,3,5,6,8,10,11,12,13,14,15),
                                     "1 2 1 3" = c(2,3,5,6,7,8,9,10,11,12,13,14,15),
                                     "1 2 2 1" = c(2,3,4,7,8,9,11,12,13,14,15),
                                     "1 2 2 2" = c(2,4,5,8,9,10,11,12,13,15),
                                     "1 2 2 3" = c(2,3,4,5,7,8,9,10,11,12,13,14,15),
                                     "1 2 3 1" = c(2,3,4,6,7,8,9,10,11,12,13,14,15),
                                     "1 2 3 2" = c(2,3,4,5,6,8,9,10,11,12,13,14,15),
                                     "1 2 3 3" = c(2,4,5,6,7,8,9,10,11,12,13,14,15),
                                     "1 2 3 4" = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15))
  
ImpossibleRelatedness <- c(ImpossibleRelatednessCrossing,ImpossibleRelatednessPop)


GenoChar <- paste(HybridGenom[,1],HybridGenom[,2],HybridGenom[,3],HybridGenom[,4],sep='')
IBS <- strtoi(GenoChar, base = 2)
ListProbCond <- lapply(0:15 , function(x) .VecProbCondPhased(FreqPop,x,(IBS==x)))

DeltaInit <- matrix(runif(15*NbInit,min=0,max=1),NbInit,15)
#DeltaInit[,8] <- DeltaInit[,2]*DeltaInit[,3]/DeltaInit[,1]
#DeltaInit[,9] <- DeltaInit[,4]*DeltaInit[,7]/DeltaInit[,1] 
#DeltaInit[,10] <- DeltaInit[,5]*DeltaInit[,6]/DeltaInit[,1] 
DeltaInit[,ImpossibleRelatedness] <- 0
DeltaInit <- DeltaInit/rowSums(DeltaInit)
NbConfIBD <- 15
}else{


  
ImpossibleRelatednessCrossing <- switch(VecCrossChar,
                                          "1 1 1 1" = c(1,2,3,4,5,6,7,8),
                                          "1 1 1 2" = c(1,2,3,4,5,6,8),
                                          "1 1 2 1" = c(1,2,3,4,5,6,8),
                                          "1 1 2 2" = c(1,2,3,4,6,7,8),
                                          "1 1 2 3" = c(1,3,4,6,8),
                                          "1 2 1 1" = c(1,2,3,4,5,6,7),
                                          "1 2 1 2" = c(1,2,3,4,5,7,8),
                                          "1 2 1 3" = c(1,2,3,5),
                                          "1 2 2 1" = c(1,2,3,4,5,7,8),
                                          "1 2 2 2" = c(1,2,3,4,5,6,7),
                                          "1 2 2 3" = c(1,2,3,5),
                                          "1 2 3 1" = c(1,2,3,5),
                                          "1 2 3 2" = c(1,2,3,5),
                                          "1 2 3 3" = c(1,2,4,8),
                                          "1 2 3 4" = c())
  
ImpossibleRelatednessPop <- switch(LinePopChar,
                                     "1 1 1 1" = c(),
                                     "1 1 1 2" = c(3,5,6,8,9),
                                     "1 1 2 1" = c(3,5,6,8,9),
                                     "1 1 2 2" = c(4,6,7,8,9),
                                     "1 1 2 3" = c(3,4,5,6,7,8,9),
                                     "1 2 1 1" = c(2,5,6,7,9),
                                     "1 2 1 2" = c(2,3,5,7,8,9),
                                     "1 2 1 3" = c(2,3,5,6,7,8,9),
                                     "1 2 2 1" = c(2,3,5,7,8,9),
                                     "1 2 2 2" = c(2,5,6,7,9),
                                     "1 2 2 3" = c(2,3,5,6,7,8,9),
                                     "1 2 3 1" = c(2,3,5,6,7,8,9),
                                     "1 2 3 2" = c(2,3,5,6,7,8,9),
                                     "1 2 3 3" = c(2,4,5,6,7,8,9),
                                     "1 2 3 4" = c(2,3,4,5,6,7,8,9))
  
ImpossibleRelatedness <- c(ImpossibleRelatednessCrossing,ImpossibleRelatednessPop)


HybGen <- matrix(NA,NbSnp,2)
HybGen[,1] <- rowSums(HybridGenom[,c(1,2)])
HybGen[,2] <- rowSums(HybridGenom[,c(3,4)])
IBS <- sapply(1:NbSnp , function(x) .ClassIBSUnphased(HybGen[x,]))
ListProbCond <- lapply(0:8 , function(x) .VecProbCondUnphased(FreqPop,x,(IBS==x),LinePop))

DeltaInit <- matrix(runif(9*NbInit,min=0,max=1),NbInit,9)
#DeltaInit[,5] <- DeltaInit[,2]*DeltaInit[,3]/DeltaInit[,1]
DeltaInit[,ImpossibleRelatedness] <- 0
DeltaInit <- DeltaInit/rowSums(DeltaInit)
NbConfIBD <- 9
}
ProbCond <- Reduce("+",ListProbCond)

List <- sapply(1:NbInit, function(x) .EM(ProbCond,DeltaInit[x,],Proj,Prec))
LogV <- sapply(1:NbInit, function(x) .LogVrai(matrix(List[7,][[x]],NbSnp,NbConfIBD,byrow=TRUE)))
DeltaEst <- List[6,][[which.max(LogV)]]
LogVEst <- max(LogV)
return(list(Delta=DeltaEst,LogV=LogVEst))
}
.TheFifteenDeltaGraph <-
function(){
dev.new()
layout(matrix(seq(1,15),byrow=TRUE,3,5),1,1)
plot(c(0,1,0,1),c(0,0,1,1),type='p',main='Delta1',xlab='Gamete',ylab='Hybride',yaxt='n',xaxt='n')
  
plot(c(0,1,0,1),c(0,0,1,1),type='p',main='Delta2',xlab='Gamete',ylab='Hybride',yaxt='n',xaxt='n')
lines(c(0,1),c(1,1),col='red')
  
plot(c(0,1,0,1),c(0,0,1,1),type='p',main='Delta3',xlab='Gamete',ylab='Hybride',yaxt='n',xaxt='n')
lines(c(0,1),c(0,0),col='red')
  
plot(c(0,1,0,1),c(0,0,1,1),type='p',main='Delta4',xlab='Gamete',ylab='Hybride',yaxt='n',xaxt='n')
lines(c(0,0),c(0,1),col='red')
  
plot(c(0,1,0,1),c(0,0,1,1),type='p',main='Delta5',xlab='Gamete',ylab='Hybride',yaxt='n',xaxt='n')
lines(c(0,1),c(1,0),col='red')
  
plot(c(0,1,0,1),c(0,0,1,1),type='p',main='Delta6',xlab='Gamete',ylab='Hybride',yaxt='n',xaxt='n')
lines(c(0,1),c(0,1),col='red')
  
plot(c(0,1,0,1),c(0,0,1,1),type='p',main='Delta7',xlab='Gamete',ylab='Hybride',yaxt='n',xaxt='n')
lines(c(1,1),c(0,1),col='red')
  
plot(c(0,1,0,1),c(0,0,1,1),type='p',main='Delta8',xlab='Gamete',ylab='Hybride',yaxt='n',xaxt='n')
lines(c(0,1),c(1,1),col='red')
lines(c(0,1),c(0,0),col='red')

plot(c(0,1,0,1),c(0,0,1,1),type='p',main='Delta9',xlab='Gamete',ylab='Hybride',yaxt='n',xaxt='n')
lines(c(0,0),c(0,1),col='red')
lines(c(1,1),c(0,1),col='red')
  
plot(c(0,1,0,1),c(0,0,1,1),type='p',main='Delta10',xlab='Gamete',ylab='Hybride',yaxt='n',xaxt='n')
lines(c(0,1),c(0,1),col='red')
lines(c(0,1),c(1,0),col='red')
  
plot(c(0,1,0,1),c(0,0,1,1),type='p',main='Delta11',xlab='Gamete',ylab='Hybride',yaxt='n',xaxt='n')
lines(c(0,1),c(0,1),col='red')
lines(c(0,0),c(0,1),col='red')
lines(c(0,1),c(1,1),col='red')

plot(c(0,1,0,1),c(0,0,1,1),type='p',main='Delta12',xlab='Gamete',ylab='Hybride',yaxt='n',xaxt='n')
lines(c(0,1),c(1,0),col='red')
lines(c(1,1),c(0,1),col='red')
lines(c(0,1),c(1,1),col='red')
  
plot(c(0,1,0,1),c(0,0,1,1),type='p',main='Delta13',xlab='Gamete',ylab='Hybride',yaxt='n',xaxt='n')
lines(c(0,1),c(1,0),col='red')
lines(c(0,0),c(0,1),col='red')
lines(c(0,1),c(0,0),col='red')
  
plot(c(0,1,0,1),c(0,0,1,1),type='p',main='Delta14',xlab='Gamete',ylab='Hybride',yaxt='n',xaxt='n')
lines(c(0,1),c(0,1),col='red')
lines(c(1,1),c(0,1),col='red')
lines(c(0,1),c(0,0),col='red')
  
plot(c(0,1,0,1),c(0,0,1,1),type='p',main='Delta15',xlab='Gamete',ylab='Hybride',yaxt='n',xaxt='n')
lines(c(0,1),c(0,1),col='red')
lines(c(0,0),c(0,1),col='red')
lines(c(0,1),c(1,1),col='red')
lines(c(0,1),c(0,0),col='red')
lines(c(0,1),c(1,0),col='red')
lines(c(1,1),c(0,1),col='red')
}
.TheNineDeltaGraph <-
function(){  
dev.new()
layout(matrix(seq(1,9),byrow=TRUE,3,3),1,1)
plot(c(0,1,0,1),c(0,0,1,1),type='p',main='Delta1',xlab='Gamete',ylab='Hybrids',yaxt='n',xaxt='n')

plot(c(0,1,0,1),c(0,0,1,1),type='p',main='Delta2',xlab='Gamete',ylab='Hybrids',yaxt='n',xaxt='n')
lines(c(0,1),c(1,1),col='red')

plot(c(0,1,0,1),c(0,0,1,1),type='p',main='Delta3',xlab='Gamete',ylab='Hybrids',yaxt='n',xaxt='n')
lines(c(0,1),c(0,0),col='red')

plot(c(0,1,0,1),c(0,0,1,1),type='p',main='Delta4',xlab='Gamete',ylab='Hybrids',yaxt='n',xaxt='n')
lines(c(0,0),c(0,1),col='red')

plot(c(0,1,0,1),c(0,0,1,1),type='p',main='Delta5',xlab='Gamete',ylab='Hybrids',yaxt='n',xaxt='n')
lines(c(0,1),c(0,0),col='red')
lines(c(0,1),c(1,1),col='red')

plot(c(0,1,0,1),c(0,0,1,1),type='p',main='Delta6',xlab='Gamete',ylab='Hybrids',yaxt='n',xaxt='n')
lines(c(0,0),c(0,1),col='red')
lines(c(1,1),c(0,1),col='red')

plot(c(0,1,0,1),c(0,0,1,1),type='p',main='Delta7',xlab='Gamete',ylab='Hybrids',yaxt='n',xaxt='n')
lines(c(0,1),c(1,1),col='red')
lines(c(0,1),c(0,1),col='red')
lines(c(0,0),c(0,1),col='red')

plot(c(0,1,0,1),c(0,0,1,1),type='p',main='Delta8',xlab='Gamete',ylab='Hybrids',yaxt='n',xaxt='n')
lines(c(0,1),c(0,0),col='red')
lines(c(0,1),c(1,0),col='red')
lines(c(0,0),c(0,1),col='red')


plot(c(0,1,0,1),c(0,0,1,1),type='p',main='Delta9',xlab='Gamete',ylab='Hybrids',yaxt='n',xaxt='n')
lines(c(0,1),c(0,0),col='red')
lines(c(0,1),c(1,1),col='red')
lines(c(0,0),c(0,1),col='red')
lines(c(1,1),c(0,1),col='red')
lines(c(0,1),c(0,1),col='red')
lines(c(0,1),c(1,0),col='red')
}
.VecProbCondPhased <-
function(Freq , IBS , Position){
FreqIBS <- Freq*Position
q1 <- Position-FreqIBS[,1]
p1 <- FreqIBS[,1]
q2 <- Position-FreqIBS[,2]
p2 <- FreqIBS[,2]
q3 <- Position-FreqIBS[,3]
p3 <- FreqIBS[,3]
q4 <- Position-FreqIBS[,4]
p4 <- FreqIBS[,4]
IBS <- as.character(IBS)
  
res <- switch(IBS,
                "0" = cbind(q1*q2*q3*q4 , q1*q3*q4 , q1*q2*q3 , q1*q2*q4 , q1*q2*q3 , q1*q2*q4 , q1*q2*q3 , q1*q3 , q1*q2 , q1*q2 , q1*q4 , q1*q3 , q1*q2 , q1*q2 , q1),
                "1" = cbind(q1*q2*q3*p4 , q1*q3*p4 , 0 , q1*q2*p4 , 0 , q1*q2*p4 , 0 , 0 , 0 , 0 , q1*p4 , 0 , 0 , 0 , 0),
                "2" = cbind(q1*q2*p3*q4 , q1*p3*q4 , 0 , 0 , q1*q2*p3 , 0 , q1*q2*p3 , 0 , 0 , 0 , 0 , q1*p3 , 0 , 0 , 0),
                "3" = cbind(q1*q2*p3*p4 , q1*p3*p4 , q1*q2*p3 , 0 , 0 , 0 , 0 , q1*p3 , 0 , 0 , 0 , 0 , 0 , 0 , 0),
                "4" = cbind(q1*p2*q3*q4 , 0 , q1*p2*q3 , q1*p2*q4 , q1*p2*q3 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , q1*p2 , 0 , 0),
                "5" = cbind(q1*p2*q3*p4 , 0 , 0 , q1*p2*p4 , 0 , 0 , q1*p2*q3 , 0 , q1*p2 , 0 , 0 , 0 , 0 , 0 , 0),
                "6" = cbind(q1*p2*p3*q4 , 0 , 0 , 0 , q1*p2*p3 , q1*p2*q4 , 0 , 0 , 0 , q1*p2 , 0 , 0 , 0 , 0 , 0),
                "7" = cbind(q1*p2*p3*p4 , 0 , q1*p2*p3 , 0 , 0 , q1*p2*p4 , q1*p2*p3 , 0 , 0 , 0 , 0 , 0 , 0 , q1*p2 , 0),
                "8" = cbind(p1*q2*q3*q4 , 0 , p1*q2*q3 , 0 , 0 , p1*q2*q4 , p1*q2*q3 , 0 , 0 , 0 , 0 , 0 , 0 , p1*q2 , 0),
                "9" = cbind(p1*q2*q3*p4 , 0 , 0 , 0 , p1*q2*q3 , p1*q2*p4 , 0 , 0 , 0 , p1*q2 , 0 , 0 , 0 , 0 , 0),
                "10" = cbind(p1*q2*p3*q4 , 0 , 0 , p1*q2*q4 , 0 , 0 , p1*q2*p3 , 0 , p1*q2 , 0 , 0 , 0 , 0 , 0 , 0),
                "11" = cbind(p1*q2*p3*p4 , 0 , p1*q2*p3 , p1*q2*p4 , p1*q2*p3 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , p1*q2 , 0 , 0),
                "12" = cbind(p1*p2*q3*q4 , p1*q3*q4 , p1*p2*q3 , 0 , 0 , 0 , 0 , p1*q3 , 0 , 0 , 0 , 0 , 0 , 0 , 0),
                "13" = cbind(p1*p2*q3*p4 , p1*q3*p4 , 0 , 0 , p1*p2*q3 , 0 , p1*p2*q3 , 0 , 0 , 0 , 0 , p1*q3 , 0 , 0 , 0),
                "14" = cbind(p1*p2*p3*q4 , p1*p3*q4 , 0 , p1*p2*q4 , 0 , p1*p2*q4 , 0 , 0 , 0 , 0 , p1*q4 , 0 , 0 , 0 , 0),
                "15" = cbind(p1*p2*p3*p4 , p1*p3*p4 , p1*p2*p3 , p1*p2*p4 , p1*p2*p3 , p1*p2*p4 , p1*p2*p3 , p1*p3 , p1*p2 , p1*p2 , p1*p4 , p1*p3 , p1*p2 , p1*p2 , p1))
 
return(res)
}
.VecProbCondUnphased <-
function(Freq , IBS , Position , LinePop){
FreqIBS <- Freq*Position
q1 <- Position-FreqIBS[,1]
p1 <- FreqIBS[,1]
q2 <- Position-FreqIBS[,2]
p2 <- FreqIBS[,2]
q3 <- Position-FreqIBS[,3]
p3 <- FreqIBS[,3]
q4 <- Position-FreqIBS[,4]
p4 <- FreqIBS[,4]
IBS <- as.character(IBS)
  
res <- switch(IBS,
"0" = cbind(q1*q2*q3*q4 , q1*q3*q4 , q1*q2*q3 , q1*q2*q4*(LinePop[1]==LinePop[3]) + q1*q2*q3*(LinePop[1]==LinePop[4]) + q1*q2*q4*(LinePop[2]==LinePop[3]) + q1*q2*q3*(LinePop[2]==LinePop[4]) , q1*q3 , q1*q2*((LinePop[1]==LinePop[3])*(LinePop[2]==LinePop[4]) + (LinePop[1]==LinePop[4])*(LinePop[2]==LinePop[3])) , q1*q4*(LinePop[1]==LinePop[3]) + q1*q3*(LinePop[1]==LinePop[4]) , q1*q2*(LinePop[2]==LinePop[3]) + q1*q2*(LinePop[1]==LinePop[3]) , q1),
                "1" = cbind(q1*q2*q3*p4 + q1*q2*p3*q4 , q1*q3*p4 + q1*p3*q4 , 0 , q1*q2*p4*(LinePop[1]==LinePop[3]) + q1*q2*p3*(LinePop[1]==LinePop[4]) + q1*q2*p4*(LinePop[2]==LinePop[3]) + q1*q2*p3*(LinePop[2]==LinePop[4]) , 0 , 0 , q1*p4*(LinePop[1]==LinePop[3]) + q1*p3*(LinePop[1]==LinePop[4]) , 0 , 0),
                "2" = cbind(q1*q2*p3*p4 , q1*p3*p4 , q1*q2*p3 , 0 , q1*p3 , 0 , 0 , 0 , 0),
                "3" = cbind(p1*q2*q3*q4 + q1*p2*q3*q4 , 0 , q1*p2*q3 + p1*q2*q3 , q1*p2*q4*(LinePop[1]==LinePop[3]) + q1*p2*q3*(LinePop[1]==LinePop[4]) + p1*q2*q4*(LinePop[2]==LinePop[3]) + p1*q2*q3*(LinePop[2]==LinePop[4]) , 0 , 0 , 0 , q1*p2*(LinePop[1]==LinePop[3]) + p1*q2*(LinePop[2]==LinePop[3]) , 0),
                "4" = cbind(q1*p2*q3*p4 + q1*p2*p3*q4 + p1*q2*q3*p4 + p1*q2*p3*q4 , 0 , 0 , (q1*p2*p4 + p1*q2*q4)*(LinePop[1]==LinePop[3]) + (q1*p2*p3 + p1*q2*q3)*(LinePop[1]==LinePop[4]) + (q1*p2*q4 + p1*q2*p4)*(LinePop[2]==LinePop[3]) + (q1*p2*q3 + p1*q2*p3)*(LinePop[2]==LinePop[4]) , 0 , (q1*p2 + p1*q2)*(LinePop[1]==LinePop[3])*(LinePop[2]==LinePop[4]) + (q1*p2 + p1*q2)*(LinePop[1]==LinePop[4])*(LinePop[2]==LinePop[3]) , 0 , 0 , 0),
                "5" = cbind(q1*p2*p3*p4 + p1*q2*p3*p4 , 0 , q1*p2*p3 + p1*q2*p3 , p1*q2*p4*(LinePop[1]==LinePop[3]) + p1*q2*p3*(LinePop[1]==LinePop[4]) + q1*p2*p4*(LinePop[2]==LinePop[3]) + q1*p2*p3*(LinePop[2]==LinePop[4]) , 0 , 0 , 0 , p1*q2*(LinePop[1]==LinePop[3]) + q1*p2*(LinePop[2]==LinePop[3]) , 0),
                "6" = cbind(p1*p2*q3*q4 , p1*q3*q4 , p1*p2*q3 , 0 , p1*q3 , 0 , 0 , 0 , 0),
                "7" = cbind(p1*p2*q3*p4 + p1*p2*p3*q4 , p1*q3*p4 + p1*p3*q4 , 0 , p1*p2*q4*(LinePop[1]==LinePop[3]) + p1*p2*q3*(LinePop[1]==LinePop[4]) + p1*p2*q4*(LinePop[2]==LinePop[3]) + p1*p2*q3*(LinePop[2]==LinePop[4]) , 0 , 0 , p1*q4*(LinePop[1]==LinePop[3]) + p1*q3*(LinePop[1]==LinePop[4]) , 0 , 0),
                "8" = cbind(p1*p2*p3*p4 , p1*p3*p4 , p1*p2*p3 , p1*p2*p4*(LinePop[1]==LinePop[3]) + p1*p2*p3*(LinePop[1]==LinePop[4]) + p1*p2*p4*(LinePop[2]==LinePop[3]) + p1*p2*p3*(LinePop[2]==LinePop[4]) , p1*p3 , p1*p2*((LinePop[1]==LinePop[3])*(LinePop[2]==LinePop[4]) + (LinePop[1]==LinePop[4])*(LinePop[2]==LinePop[3])) , p1*p4*(LinePop[1]==LinePop[3]) + p1*p3*(LinePop[1]==LinePop[4]) , p1*p2*(LinePop[2]==LinePop[3]) + p1*p2*(LinePop[1]==LinePop[3]) , p1))


res[,4] <- res[,4]/max((LinePop[1]==LinePop[3])+(LinePop[1]==LinePop[4])+(LinePop[2]==LinePop[3])+(LinePop[2]==LinePop[4]),1)
res[,6] <- res[,6]/max((LinePop[1]==LinePop[3])*(LinePop[2]==LinePop[4])+(LinePop[2]==LinePop[3])*(LinePop[1]==LinePop[4]),1)
res[,7] <- res[,7]/max((LinePop[1]==LinePop[3])+(LinePop[1]==LinePop[4]),1)
res[,8] <- res[,8]/max((LinePop[1]==LinePop[3])+(LinePop[2]==LinePop[3]),1)
return(res)
}
