### **Libraries**

The libraries we used to build our RShiny project are listed below:

```{r}
library(tidyverse)
library(ggcorrplot)
library(rvest)
library(repurrrsive)
library(shiny)
library(shinyWidgets)
library(plotly) #used to create our interactive scatterplot (i.e., when you mouse over it, you can read the values)
library(bslib) #used for selecting an RShiny theme using Bootstrap and Bootswatches. We used the theme "vapor"
library(markdown)
```

### **Code Troubleshooting**

The following StackOverflow threads (and other sites) helped us:

[1. Pull pictures from the web](https://stackoverflow.com/questions/53147225/display-images-from-web-in-shiny-r)

[2. Adding local images to RShiny application](https://stackoverflow.com/questions/19434991/adding-local-image-with-html-to-a-shiny-app)

[3. Embedding .md files into RShiny](https://gist.github.com/wch/9744711)

[4. Dynamic images based on user input (MOST HELPFUL!)](https://stackoverflow.com/questions/41620721/dynamic-image-output-based-on-user-input-shiny-r)

### **Web-Scraping Project**

Our Shiny App is based off of the data we scraped for our graduate student project. Our app allows users to explore generation I Pokémon statistics based on Pokémon types.

Below, is the code we used to web-scrape our data:

```{r}
Link <- "https://pokemondb.net/pokedex/stats/gen1"

page <- read_html(Link)

Name <- page%>% html_nodes(".cell-name") %>% html_text()

Type <- page%>% html_nodes(".cell-icon") %>% html_text()

Total <- page%>% html_nodes(".cell-total") %>% html_text()

Attack <- page%>% html_nodes(".cell-num:nth-child(6)") %>% html_text()

Defense <- page%>% html_nodes(".cell-num:nth-child(7)") %>% html_text()

Speed <- page%>% html_nodes(".cell-num:nth-child(10)") %>% html_text()

SpAtk <- page %>% html_nodes(".cell-num:nth-child(8)") %>% html_text()

SpDef <- page %>% html_nodes(".cell-num:nth-child(9)") %>% html_text()

pokemondataframe_g1 <- data.frame(Name, Type, Total, Attack, Defense, SpAtk, SpDef, Speed)

as_tibble(pokemondataframe_g1)

```

Next, we cleaned up our data types. We recoded all statistics coded as characters to doubles, fixed string errors, and created ID numbers, which weren't given to us in the original data. The cleaning is in the code chunk below:

```{r}
#recode character variables to doubles because they are continuous, numerical variables
pokemon_df_clean <- pokemondataframe_g1 %>%
  mutate(Total = as.numeric(Total),
         Attack = as.numeric(Attack),
         Defense = as.numeric(Defense),
         Speed = as.numeric(Speed),
         SpAtk = as.numeric(SpAtk),
         SpDef = as.numeric(SpDef))

#parse Pokémon with two types into two columns
Pokémon_df_clean$Type <- gsub("(?!^)(?=[[:upper:]])", ".", Pokémon_df_clean$Type, perl=T)

pokemon <- Pokémon_df_clean %>% separate(Type, c("Primary_Type", "Secondary_Type"), extra = "merge", fill = "right")

#recode the types as factors so ggplot will read them correctly
pokemontypes <- c("Grass", "Poison", "Fire", "Flying", "Water", "Bug", "Normal", "Electric", "Ground", "Fighting", "Fairy", "Psychic", "Rock", "Steel", "Ice", "Ghost", "Dragon")

pokemon$Primary_Type <- factor(Pokémon$Primary_Type, levels = Pokémontypes)

pokemon$Secondary_Type <- factor(Pokémon$Secondary_Type, levels = Pokémontypes)

pokemon
ID <- formatC(ID_no, width = 3, format = "d", flag = "0")

pokemon$ID <- ID

pokemon1 <- Pokémon

pokemon$Name <- tolower(Pokémon$Name) #we needed a dataframe with the names in lowercase because in order for our RShiny app to integrate with the website we used for the pictures widget, the link needed lowercase names.

pokemon

```

We created the following plots during the exploratory analysis of our web-scraped data:

### **Exploratory Data Analysis**

We created the following plots during the exploratory analysis of our web-scraped data:

**1. Of the original 151 Pokémon, what is the most common primary type? What is the least common primary type?**

```{r}
object <- ggplot(pokemon, mapping = aes(x = Primary_Type, fill = Primary_Type)) +
  geom_bar(stat = "count") +
  ggtitle("Counts of Primary Types in the First Generation of Pokémon") +
  xlab("Primary Type") +
  scale_fill_discrete(drop = FALSE) +
  theme_bw()

ggplotly(object)
```

Water is the most common primary type. Fair and Ice tie for the least common primary type.

**2. Is there a relationship between special attack (SpAtk) and attack? What about special defense (SpDef) and defense?**

```{r}
object2 <- ggplot(pokemon, mapping = aes(x = SpAtk, y = Attack)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("The relationship between Special Attack and Attack") +
  theme_bw()

ggplotly(object2)
```

There does not appear to be a strong, linear relationship between attack and special attack.

```{r}
object3 <- ggplot(pokemon, mapping = aes(x = SpDef, y = Defense)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "Red") +
  ggtitle("The relationship between Special Defense and Defense") +
  theme_bw()

ggplotly(object3)
```

There does not appear to be a strong, linear relationship between defense and special defense.

**3. Is there a relationship between any of our numeric variables?**

```{r}
pokemoncorr <- pokemon %>%
  select(-Name, -Primary_Type, -Secondary_Type)

corr <- round(cor(Pokémoncorr), 2)

p.mat <- cor_pmat(Pokémoncorr)

ggcorrplot(corr, hc.order = TRUE, type = "lower",
   lab = TRUE)
```

According to the correlation plot above, as any of the statistical measures of Pokémon strength increases, the other measures tend to be higher as well. We see strong correlations particularly between Total and any other statistic, which makes sense, as Total is calculated by adding all of the statistical measures of one Pokémon together.

```{r}
object4 <- ggplot(pokemon, mapping = aes(x = Total, fill = Primary_Type)) +
  geom_boxplot() +
  facet_wrap(~ Primary_Type) +
  ggtitle("Total Stat Distribution by Type") +
  theme_bw() +
  theme(legend.position = "none")

ggplotly(object4)
```

According to the boxplots, ice type Pokémon appear to be the "strongest" type based on average total base points. However, this could also be because there are not many ice type Pokémon.
