# -*- coding: utf-8 -*-
"""
Created on Tue Apr 16 17:28:52 2019

@author: Mohamed Ibrahim
"""

import gmail
import yagmail
import sys
import re

GMAIL_USERNAME = 'mabolfadl@gmail.com'
GMAIL_PASSWORD = 'Hamadaguc_2010'

g = gmail.login(GMAIL_USERNAME, GMAIL_PASSWORD)

if not g.logged_in:
    sys.exit()

msgs = g.inbox().mail(sender="kumar.a@example.com", unread=True, prefetch=True)

pattern = re.compile("\bsorry\b | \bhelp\b | \bwrong\b ", flags=re.I)

for msg in msgs:
    if pattern.match(msg.body):
        msg.label("Database fixes")
        msg.reply("No problem. I've fixed it. \n\n Please be careful next time.")
        
        
        
        
yagmail.register(GMAIL_USERNAME, GMAIL_PASSWORD)

yagmail.SMTP(GMAIL_USERNAME).send(GMAIL_USERNAME, 'test', 'This is the body')

def send_reply(subject):
    yag = yagmail.SMTP(GMAIL_USERNAME)
    yag.send(
        to=KUMAR_EMAIL,
        subject='RE: {}'.format(subject),
        contents=REPLY_BODY,
    )




g = gmail.login(GMAIL_USERNAME, GMAIL_PASSWORD)
for mail in g.inbox().mail(unread=True, sender=KUMAR_EMAIL, prefetch=True):
        if KEYWORDS_REGEX.search(mail.body):
            # Restore DB and send a reply.
            mail.add_label('Database fixes')
            send_reply(mail.subject)