library(multcomp)
library(car) # levene test 
library(jmv)

# import datasets 
problem_1 <- read_excel("problems.xlsx", sheet = "problem 1")
problem_3 <- read_excel("problems.xlsx", sheet = "problem 3")
problem_4 <- read_excel("problems.xlsx", sheet = "problem 4")

View(problem_1)
View(problem_3)
View(problem_4)

# problem 1
model_p1 <- aov(test ~ temp, data = problem_1) 
summary(model_p1)
print(model_p1)

# https://www.chegg.com/homework-help/test-ability-certain-polymer-remove-toxic-wastes-water-exper-chapter-10-problem-7p-solution-9780123948113-exc

# problem 2

gmean <- (32+40+30)/3 # grand mean 
SSTR<-(12*((32-gmean)^2+(40-gmean)^2+(30-gmean)^2)) # Sum of Squares treatment
MSTR<-SSTR/(3-1)
SSe<-(12-1)*(33+44+40) # Sum of Squares error
MSE<-SSe/(36-3)
F<-MSTR/MSE # f-test 
P <- 1-pf(8.6154,2,33) # p-value 
q<-3.48
MOE<-3.48*sqrt(MSE/12)



cat(paste("F-test = ",round(F, digits = 5), " p-value = ", round(P, digits = 5),'\n',
          'CI of A-B: (',round(32-40-MOE, digits = 5),round(32-40+MOE,digits = 5),')\n',
      'CI of A-C: (',round(32-30-MOE, digits = 5),round(32-30+MOE,digits = 5),')\n',
      'CI of B-C: (',round(40-30-MOE, digits = 5),round(40-30+MOE,digits = 5),')'))


# https://www.chegg.com/homework-help/emergency-room-physician-wanted-know-whether-differences-amo-chapter-10-problem-12p-solution-9780123948113-exc
# https://www.chegg.com/homework-help/questions-and-answers/problem-2-submit-r-notebook-comments-code-results-discussions-emergency-room-physician-wan-q87326311

# problem 3 

model_p3 <- aov(Yield ~ Area, data = problem_3)
summary(model_p3)
print(model_p3)


# https://www.chegg.com/homework-help/questions-and-answers/submit-r-notebook-comments-code-results-discussions-problem-3-answer-needed-r-studio-code--q87785960

# problem 4

problem_4$Area <- as.factor(problem_4$Area)  

model_p4 <- aov(Yield ~ `Height(inches)`+ Area    , data = problem_4)
leveneTest(`Height(inches)`~Area, data = problem_4)
Anova(model_p4, type="III")
summary.lm(model_p4)
postHocs <- glht(model_p4, linfct = mcp(Area = "Tukey"))
summary(postHocs)
confint(postHocs)
print(model_p4)

ancova(data = problem_4, dep =  Yield, factors = Area, covs = 'Height(inches)',postHoc = ~ Area, modelTest = T)


# https://www.r-bloggers.com/2021/07/how-to-perform-ancova-in-r/


sqrt(1.5449376923077)
