# Function of neo4j time course shiny
This repository is for using in neo4j time course database.
During this shiny, you can select one pathway to see genes relationships via WGCNA over time.

# How to run

**step1**   
Before you run this shiny, you need to start neo4j database and using my database, so you should ask me give you this database.  
Also, you should make http://localhost:7474/ as intrerface website and xiaowei as password and neo4j as username. Or not, you need to change
my codes at connected neo4j.

**step2**  
Run this codes:
```
library(shiny)
runGitHub("xiaowei3223/neo4j_time_course_shiny")
```

# This shiny interface shows like as following:

![interface_neo4j_time_course_shiny.png](./img/interface_neo4j_time_course_shiny.jpg)
