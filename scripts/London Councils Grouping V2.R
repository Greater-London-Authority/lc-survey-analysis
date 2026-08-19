# Survey questions (label=long)

SURVEY_QUESTIONS <-
  c(
    'awareness__mc1'='Awareness of public services',
    'access__mat1'='Frequency of public services use',
    'satisfaction__sc1'='Satisfaction of public services',
    'diffsame__mat2' = 'Use of public services for different or same reasons',
    'reason__mc2'='Reason for use of public services',
    'resolve__sc2'='Whether services resolved issues',
    'satisfaction__mat3'='Satisfaction with support',
    'dissatisfaction__cx1'='Reasons for dissatisfaction with support',
    'influence__sc3'='Influence on services',
    'interestimp__sc4'='Interest in improving services',
    'difficult__mc3'='Barriers to accessing services',
    'important__mat4'='Important factors in services',
    'community__mc4'='Community strengths',
    'commstrengths__sc5'='Recognition of community strengths in services',
    'commeasier__mc5'='Actions to make access easier',
    'tec__mc6'='Use of digital platforms',
    'confidence__sc6'='Confidence in digital platforms',
    'tecbarriers__mc7'='Barriers to using digital platforms',
    'access__mc8'='Alternative actions taken against digital access',
    'tecwell__sc7'='Perceptions of digital platforms usability',
    'whydifficult__mc9'='Reasons for difficulties when using digital platforms',
    'tecdata__sc8'='Confidence in technology within public services',
    'imptec__open1'= 'Use of technology in improving',
    'AIsupp__sc9'='AI in public services',
    'happierhealthier__mc10'='What would make Londoners happier and healthier',
    'disability__sc10'='SDisability Status',
    'involve__sc11'='Involvement in future research'
  )

# Single choice value levels
SC_VALUE_LEVELS <- list(
  c("Very satisfied", "Somewhat satisfied", "Somewhat dissatisfied", "Very dissatisfied", "Don't know"),
  c("Strongly agree", "Tend to agree", "Tend to disagree", "Strongly disagree"),
  c("A lot", "A little", "Not a lot", "Not at all", "Don't know"),
  c("Very interested", "Fairly interested", "Not very interested", "Not at all interested", "Don't know"),
  c("A lot", "A little", "Not a lot", "Not at all", "Don't know"),
  c("Very confident", "Somewhat confident", "Not very confident", "Not confident at all", "Not applicable – I do not use technology/digital channels to access public services"),
  c("Very well", "Quite well", "Not very well", "Not at all well", "Don't know"),
  c("Very confident", "Somewhat confident", "Not very confident", "Not at all confident", "Don't know"),
  c("Strongly support", "Somewhat support", "Somewhat oppose", "Strongly oppose", "Don't know"),
  c("Yes", "No", "Prefer not to say"),
  c("Yes", "No")
)

# Mat value levels (left)
MAT_VALUE_LEVELS_LEFT <- list(
  c('Seeing a doctor or getting healthcare ', 'Getting help to stay safe and well ', 'Finding or keeping a home', 'Education, childcare and support for children and young people', 'Getting around London', 'Emergency help and community safety', 'Keeping neighbourhoods clean and working well', 'Help to live healtheir lives', 'Services that shape the places people live'),
  c('For the same reason', 'For different reasons'),
  c('Seeing a doctor or getting healthcare ', 'Getting help to stay safe and well ', 'Finding or keeping a home', 'Education, childcare and support for children and young people', 'Getting around London', 'Emergency help and community safety', 'Keeping neighbourhoods clean and working well', 'Help to live healtheir lives', 'Services that shape the places people live'),
  c('Speed of support', 'Clear information', 'Feeling listened to / supported')
)

# Matrix value levels (right)
MAT_VALUE_LEVELS_RIGHT <- list(
  c('A few times a week', 'Once a week', 'A few times a month', 'Once a month', 'Every 1-3 months', 'Every 3-6 months', 'Every six months', 'Less often than once every six months', 'Never'),
  c('Yes', 'No'),
  c('Very satisfied', 'Somewhat satisfied', 'Somewhat dissatisfied', 'Very dissatisfied', 'Don\'t know', 'Not applicable - I did not use this service at the same time as others'),
  c('Very important', 'Quite important', 'Not very important', 'Not at all important', 'Don\'t know') #'Don\'t know / Not applicable')#, # Reminder the apostrophe used for the don't know is different to the other two above due to differences in Alchemer export
)

# Checkbox grid value levels
CX_VALUE_LEVELS <- list(
  c()
)

# Single choice NET levels
SC_NET_LEVELS <- list(
  list(
    'Very satisfied / Somewhat satisfied' = c('Very satisfied', 'Somewhat satisfied'),
    'Not very satisfied / Not at all satisfied' = 'Not very satisfied', 'Not at all satisfied'),
  'Don\'t know' = c('Don\'t know'
  ),
  list(
    'Strongly agree / Tend to agree' = c('Strongly agree', 'Tend to agree'),
    'Strongly disagree / Tend to disagree' = c('Strongly disagree', 'Tend to disagree')
  ),
  list(
    'A lot / A little' = c('A lot', 'A little'),
    'Not a lot / Not at all' = c('Not a lot', 'Not at all'),
    'Don\'t know' = c('Don\'t know')
  ),
  list(
    'Very interested / Fairly interested' = c('Very interested', 'Fairly interested'),
    'Not very interested / Not at all interested' = c('Not very interested', 'Not at all interested'),
    'Don\'t know' = c('Don\'t know')
  ),
  list(
    'A lot / A little' = c('A lot', 'A little'),
    'Not a lot / Not at all' = c('Not a lot',  'Not at all'),
    'Don\'t know' = c('Don\'t know')
),
  list(
    'Very confident / Somewhat confident' = c('Very confident', 'Somewhat confident'),
    'Not very confident / Not confident at all' = c('Not very confident', 'Not confident at all'),
'Not applicable – I do not use technology/digital channels to access public services' = c ('Not applicable – I do not use technology/digital channels to access public services')
),
  list(
    'Very well / Quite well' = c('Very well', 'Quite well'),
    'Not very well / Not at all well' = c('Not very well', 'Not at all well'),
'Don\'t know' = c('Don\'t know')
),
  list(
    'Very confident / Somewhat confident' = c('Very confident', 'Somewhat confident'),
    'Not very confident / Not at all confident' = c('Not very confident', 'Not at all confident'),
    'Don\'t know' = c('Don\'t know')
),
list(
  'Strongly support / Somewhat support' = c('Strongly support', 'Somewhat support'),
  'Strongly oppose / Somewhat oppose' = c('Somewhat oppose', 'Strongly oppose'),
  'Don\'t know' = c('Dont\'t know')
)
)
names(SC_NET_LEVELS) <- c('UseOften__sc2', 'ChangeWorkStudy__sc3', 'ImpactTime__sc4', 'Support__sc5', 'WorkplaceConsult__sc6', 'BusinessUse__sc7', 'EdAI__sc9', 'EasierHarder__sc10')

# Matrix NET levels
MAT_NET_LEVELS <- list(
  list(
    'Access at all' =c('A few times a week', 'Once a week', 'A few times a month', 'Once a month', 'Every 1-3 months', 'Every 3-6 months', 'Every six months', 'Less often than once every six months'),
    'Never' = c('Never')
  ),
  list('Yes' = c('Yes'),
       'No' = c('No')
       ),
  list('Very satisfied / Somewhat satisfied' = c('Very satisfied', 'Somewhat satisfied'),
       'Very dissatisfied / Somewhat dissatisfied' = c('Very dissatisfied', 'Somewhat dissatisfied'),
       'Not applicable - I did not use this service at the same time as others' = c('Not applicable - I did not use this service at the same time as others')
       ),
  list('Very important / Quite important' = c('Very important', 'Quite important'),
  'Not very important / Not at all important' = c('Not very important', 'Not at all important'),
  'Don\'t know' = c('Don\'t know'))
)
#RD: this is from the previous script - I dont recall what it's for ... names(NET_CATEGORIES_MAT) <- c('Confident__mat1')

# Demographic NET levels
DEMOG_NET_LEVELS <- list(
  list(
    'White' = c('English / Welsh / Scottish / Northern Irish / British', 'Irish', 'Gypsy or Irish Traveller', 'Any other White background'),
    'Mixed / Multiple ethnic groups' = c('White and Black Caribbean', 'White and Black African', 'White and Asian', 'Any other Mixed or Multiple background'),
    'Asian' = c('Indian', 'Pakistani', 'Bangladeshi', 'Chinese', 'Any other Asian background'),
    'Black' = c('African', 'Caribbean', 'Any other Black / African / Caribbean background'),
    'Other' = c('Arab', 'Latin American', 'Any other ethnic group')
  ),
  list(
    'Working' = c('Working - Full time (30+ hours)', 'Working - Part time (9-29 hours)'),
    'Not working' = c('Unemployed', 'Not working - disabled', 'Not working - looking after house/children'),
    'Retired' = c('Not working - retired'),
    'Student' = c('Student, with some part time work', 'Student, without any part time work'),
    'Other' = c('Other')
  )
)
names(DEMOG_NET_LEVELS) <- c('ethnicity', 'employment')

# Grouping columns
GROUP_VARS <- c(
  "age_group", "gender", "ethnicity",  "employment",
  "inner_outer_borough",  "WorkStudySect__sc13",
  "Qual__sc14", "Disability__sc15", "ParentalStat__sc16"
)
# can't group by an mc or mat variable - possible but will take a lot of work ti implement

# TODO Uncomment if conducting group-based analysis
GROUP_LEVELS <- list(
  c("16-24", "25-34","35-44","45-54","55-64","65+"),
  c("Man", "Woman", "Other"),
  c("White", "Black", "Asian", "Mixed / Multiple ethnic groups", "Other"),
  c("Working", "Not working", "Retired", "Student", "Other"),
  c("Inner borough", "Outer borough", "Outside of London"),
  # c("Self-employed / Freelancer / Sole-trader", "Business owner or Director (i.e. have legal responsibilities for the company)",
  #   "Employee (working for an employer)", "Apprentice or Trainee",
  #   "Student (full- or part-time)", "Unemployed - actively looking for work",
  #   "Not currently working (not looking)", "Retired", "Other", "Prefer not to say"),
  #c("Public sector", "Private sector", "Charity or not-for-profit", "Technology and digital", "Finance and professional services", "Education",
 #   "Health and social care", "Retail or hospitality", "Manufacturing or construction", "Creative industries", "Other", "Prefer not to say"),
 # c("No formal qualifications", "GCSEs or below", "A levels or equivalent", "Vocational or apprenticeship", "Undergraduate degree",
  #  "Postgraduate degree", "Doctorate", "Professional qualification", "Other (please specify):", "Prefer not to say"),
 c("Yes", "No", "Prefer not to say"),
  c("Yes", "No", "Prefer not to say")
)
names(GROUP_LEVELS) <- GROUP_VARS