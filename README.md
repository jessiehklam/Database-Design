# DATABASE DESIGN - R SHINY APP
#### The coursework is a comprehensive platform that help investors (users) manage their assets and allowing them to have multiple portfolios with different goals.
#### To create a multi-functional portfolio management system, we identified the key entities involved. Also, we identified the relationships between these entities to make it easier when creating the tables in the database.


### Entity Relationship Diagram Design
![](https://i.imgur.com/Nvg3PBg.png)
#
### App Interface

#### All the trade records are appointed to different portfolios. Hence, we placed the portfolio management features on the top of the tab list.

#### Then, trade records are displayed and maintained in the following tab because it helps to have a detailed view of the different trade records in each portfolio.

#### Next, the function of portfolios analysis is placed on the next page owing to that it requires all the data in earlier pages.

#### In the end, we provide a page for navigating between promising investments.


#
### Exhibit 1: Manage Portfolios
![](https://i.imgur.com/jLR7HWN.png)
###### Clients can create new portfolios by entering names in the first box. Old portfolios can also be deleted by keying in the existing portfolio name. Moreover, in order to prevent there are duplicate Portfolio IDs with deleted ones, portfolio IDs are desinged to being appended incrementally with the design on the back end.  
###
### Exhibit 2: Trade Records
![](https://i.imgur.com/dUz62k1.png)
###### o	Clients are able to view all the trade records filtered by their portfolio ID. Besides, they can choose the date range they would like to focus on. The data range is constrainted by the oldest and the newest transaction date according to trade records under each portfolio in defalut. After clicking the “Get Results” button, the table below will show all matched trade records.
###
### Exhibit 3: Manage Trade Records
![](https://i.imgur.com/h9sYFtj.png)
###### o	After clients place buy&sell orders, they can add them to their portfolios in the third tab. They can select the designated portfolio by the drop-down menu, then input related information in the below input field to add a transection. Addtionally, edit & delete functions are provided when they need to admend historical transaction details. 
###
#### There are four tab under “Portfolio Analysis”. See Exhibit 4, 5, 6, and 7.
### Exhibit 4: Portfolio Overview
![](https://i.imgur.com/fwlrSM0.png)
###### o	With regard to portfolio overview tab, users can read trade records row by row in the table shown in the tab
###
### Exhibit 5: Portfolio Allocation
![](https://i.imgur.com/jevjqJs.png)
###### o	How the asset is made use of will be demonstrated in the portfolio allocation tab by treemap, that is sized by the absoulte sum value of inventory for each stock.
###
### Exhibit 6: Performance
![](https://i.imgur.com/oG4dc2s.png)
###### o	As for performances tab, we can closely look at the returns in terms of amount and percentage which reflects whether they optimize the use of resource. Also, we append the shown table with relevant information for reference, including avaerge cost and current price.
###
### Exhibit 7: Long & Short
![](https://i.imgur.com/5FY7q2Q.png)
###### o	According to Long&Short tab, the waterfall chart illustrates net long or net short position. On the bottom right, the bar chart compares the difference of long and short position in total amount. 
### 
### Explore Companies
![](https://i.imgur.com/brJFqjQ.png)
###### o	In this page, users can explore new investment option. Clicking on one of company card, related information about the chosen company will pop up, helping investors making new decsion for their portfolio.
