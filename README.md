# ElectionGPT

ElectionGPT is a forecasting experiment designed to predict the outcomes of the 2024 U.S. Presidential Election using narrative-driven predictions with ChatGPT-4o-mini. This project, developed by Scott Cunningham in collaboration with Jared Black and Coco Sun, leverages innovative methods to simulate election forecasting through distinct narrative voices.

## Project Overview

The goal of this project is to explore the capabilities of large language models, like ChatGPT-4o-mini, in generating "future narratives" that predict state-by-state election outcomes. Building on previous work that predicted economic indicators and award winners, ElectionGPT focuses on predicting future events by generating stories set after the event has occurred.

## Methodology

The project employs a multi-step process:

1. Data Collection: Each day, 100 news stories related to the 2024 U.S. Presidential Election are retrieved using the Event Registry API. This approach ensures that the data is "endogenous" and not manually curated, allowing for a dynamic and unbiased selection of news sources.

2. Narrative Generation: Using the retrieved news stories, ChatGPT-4o-mini generates election result stories from the perspective of four distinct voices:
   - Direct Reporting: An anonymous, trustworthy reporter providing a "comprehensive" news story on the election outcome.
   - Fox News Reporter (Bret Baier)
   - MSNBC Reporter (Rachel Maddow)
   - BBC Reporter (Laura Kuenssberg)

   Each voice narrates the outcome of the 2024 election, creating a total of 400 stories daily (100 stories per voice).

3. Data Extraction: ChatGPT-3.5 Turbo is used to extract the winners of each state from the generated stories and store them in a matrix format. The matrix is updated daily, capturing state outcomes by distinct voice over time.

4. Data Analysis: The results are analyzed to identify patterns and deviations in predicted outcomes, exploring how different narrative voices might influence or predict election results.

## Objectives

- To assess the potential of narrative-driven predictions using AI models.
- To understand how different narrative voices, informed by distinct media biases, influence election outcome predictions.
- To build a dynamic dataset that tracks state outcomes over time and provides insights into model performance and potential biases.

## Results and Visualizations

The project will present two main types of visualizations:

1. National Time Series Plot: This plot shows the projected Democratic win as the percentage of trials each day where Kamala Harris secures enough electoral college votes to become President.

2. Map of the United States: A visual representation of daily changes in state predictions across the four distinct voices, helping to interpret variations and understand potential causes.

## Get Involved

We welcome feedback and collaboration from the community. If you're interested in contributing to the project or have suggestions, please feel free to reach out via email or leave a comment on our Substack post.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

Special thanks to Jared Black and Coco Sun for their tireless work on this project. We also appreciate the valuable input from our readers and colleagues.

---

Stay tuned for updates as we continue to refine our approach and explore the exciting potential of AI-driven election forecasting!
