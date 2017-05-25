# CS7009
CS7009 github analytics project
TASKS
Oauth
Complete.

*Crawler*
Complete.
Crawler crawls through accounts, it starts at a user, then records that users repositories, repository languages and who has starred those repositories. The process is then repeated for the users who starred the previous repositories. 
Fill Neo4j Database

NODES
- Users
- Lang
- Repo
LINKS
- isWrittenIn
- starred
- owns

Repos with the same name were allowed
Users with the same name were not allowed (because crawler had managed tto crawl back to a user that was already seen)
Lang's were unique e.g. one javaScript node.

Repos isWrittenIn Lang
Users starred Users
User owns Repo


Use D3 visualisations to render information gathered from crawling

I used two bar charts to display answers to both of my questions.

Repo Language Trends
Out of the repositories crawled, which were centered around phadej (the account which details the haskell libraries for the github API), JavaScript appears to be the most popular language. I was interested to discover this as centering the crawler around such a prominent Haskell account may result in haskell having a higher usage than what would normally be expected. This was not the case.

(and more specific to employment)

Further more I was interested to find out whether many people star their own repositories. Unsurprisingly not many people star their own repos... but there are always some outliers in this case "mcanthony", who seems to have starred 675 repositories. I investigated mcanthony further and he had a huge 4.2 k repos which perhaps makes 675 self stars slightly more understandable but still a bit unreasonable. This might be a way of promoting work he is has completed (in an employers eyes this could be regarded as dishonest)


In the Images folder screenshots of website and various other parts of the project may be seen!
