# ShinyHabitTracker
Create a custom habit tracker app with email reminders using R Shiny, Google Sheets, and cron.

## Set Up Google Sheets API
Follow the [instructions here](https://stateful.com/blog/google-sheets-api-tutorial) to set up a service account and download the JSON file with an authentification key. Once you have done that, create a Google Sheet with a "Day" column for the date and additional columns for each habit you with to track. See the example Google Sheet provided in this repo.

## Create and Customize Your App
Use the example provided in this repo to create and customize your R Shiny app. If you're new to Shiny, [this is a great tutorial](https://mastering-shiny.org/basic-app.html).

## Set Up Email Reminders
You can set up automatic email reminders using the Python script provided in this repo. You will want to edit the script to include the sender's email, recipient's email, and the link to your Shiny app. Use ```cron``` to schedule and automate the emails by running:

```
crontab -e
```

Then, add a line containing a cron expression to execute the Python script at whatever time you would like to receive an email reminder from your app. For example, if you want to be reminded every evening at 8:30pm:

```
30	20	*	*	*	/opt/homebrew/bin/python3 ~/ExampleHabitTracker/send_email.py
```

Note that the first time this cron task runs, it may ask for permission to access the directory where the Python script is. Finally, you may wish to set the machine running this task to wake up and go back to sleep just before and after sending the email. The method for doing this will depend on the machine you're using. For Mac, this can be achieved using [pmset](https://support.apple.com/guide/mac-help/schedule-your-mac-to-turn-on-or-off-mchl40376151/mac). For Windows, use [powercfg](https://learn.microsoft.com/en-us/windows-hardware/design/device-experiences/powercfg-command-line-options). 

