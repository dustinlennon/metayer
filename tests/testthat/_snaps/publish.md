# publishing context is working

    Code
      cat(result)
    Output
      ---
      title: 'metayer: examples'
      output:
        html_document:
          theme: cerulean
          highlight: espresso
          fig_caption: yes
      ---
      
      ```{r metayer='b509c5b152d4197a20d1dc5970ee0053'}
      setwd(here::here())
      devtools::load_all()
      ```
      
      ```{r metayer='f544b6207fbcb75882084b3310dfa831'}
      a <- 1:5
      tbl <- tibble::tibble(a, b = a * 2, c = 1)
      ```
      
      ```{r metayer='be6a0774d9c50f6e24c08da6ae83a55b'}
      cfplot(
        {
          plot(tbl$a, tbl$b, xlab = "foo")
        },
        width = 7,
        height = 4,
        units = "in",
        file = "foo",
        res = 96,
        bg = "azure"
      )
      ```

# preprocess raw yaml cells

    Code
      cat(json)
    Output
      {
        "title": ["updated title"],
        "output": {
          "html_document": {
            "theme": ["cerulean"],
            "highlight": ["espresso"],
            "fig_caption": [false],
            "extra_key": [1234]
          },
          "secondary_key": ["abcd"]
        },
        "author": ["Dustin Lennon\""]
      }

