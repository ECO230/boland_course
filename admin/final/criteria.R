`Basic Criteria` <- c('Does the work include at least one graph, one table, and one hypothesis test?'
,'Are the interpretations of the output provided in bullet points generally correct?'
,'Did the work use at least 2 software tools in some way?'
,'Is the graph appropriate for the type of data analyzed?'
,'Is the statistical test used appropriate for the type of data analyzed?'
,'Is the analysis related to the prompt?'
)

Yes <- c('Continue grading'
  ,'Continue grading'
  ,'Continue grading'
  ,'Continue grading'
  ,'Continue grading'
  ,'Continue grading'
)

No <- c('30/100'
,'50/100'
,'60/100'
,'65/100'
,'65/100'
,'65/100'
)

`Additional Criteria` <- c('Extent to which the work follows data visualization best practices'
,'Extent to which the organization of Excel Workbook follows best practices'
,'Extent to which the organization and annotations of R code follow best practices'
,'Extent to which interpretation(s) of statistical test(s) are clear and correct'
,'Originality of the approach'
)

`Rating Points` <- c('0-10'
        ,'0-5'
        ,'0-5'
        ,'0-5'
        ,'0-10'
)

grading_1 <- data.frame(`Basic Criteria`,Yes,No,check.names = FALSE)
grading_2 <- data.frame(`Additional Criteria`,`Rating Points`,check.names = FALSE)

save(grading_1,file='grading_1.RData')
save(grading_2,file='grading_2.RData')
