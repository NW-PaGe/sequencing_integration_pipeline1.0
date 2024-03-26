
# import the plots - formats get destroyed in outlook, but it's good enough.
# If you open the outlook email into a web browser the formatting will work.
# sum_table_email <- as_raw_html(sum_table)
# reason_table_email <- as_raw_html(reason_count_table)

sum_table_email <- add_image(file.path(project_folder,
                          "WDRS QA/Logic Checks/sum_table_image.png"),
                          width = 1000)
gisaid_plot_email <- add_ggplot(gisaid_flag_plot,width = 12,height = 7.5)

# Make the email body - make updates to email here if needed
message_body <-
  glue::glue(
    "Good Afternoon,

This is an automated email notification of the WDRS QA checks.


Here’s a summary of errors found:

{sum_table_email}

{gisaid_plot_email}


Thanks,

DIQA

ps - I'm a bot, bleep bloop"
  )
email <- blastula::compose_email(body = md(message_body))

# Sending email by SMTP using a credentials file
email %>%
  smtp_send(
    to = c(""),
    from = "",
    subject = "Sequencing QA - WDRS Logic Check Results Automated Email",
    credentials = creds_key(id = "")
  )
