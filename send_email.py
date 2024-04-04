#!/opt/homebrew/bin/python3

import smtplib
from email.mime.text import MIMEText

def send_email():
	email_sender = 'SENDER@gmail.com'
	email_recipient = 'RECIPIENT@gmail.com'
	pwd = 'PASSWORD'
	server = smtplib.SMTP('smtp.gmail.com', 587)
	server.ehlo()
	server.starttls()
	server.login(email_sender, pwd)
	email_body = """<pre> Good evening, USER! It's time to <a href="https://habittracker.shinyapps.io/examplehabittracker/">log your habits</a>
	From,
	SENDER
	</pre>"""
	message = MIMEText(email_body, 'html')
	message['Subject'] = 'Log Your Habits'
	message['From'] = email_sender
	message['To'] = email_recipient
	server.sendmail(email_sender, email_recipient, message.as_string())
	server.quit()

send_email()