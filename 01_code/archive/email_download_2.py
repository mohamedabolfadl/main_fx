# -*- coding: utf-8 -*-
"""
Created on Tue Apr 16 21:44:59 2019

@author: Mohamed Ibrahim
"""

import sys
import imaplib
import email
import email.header
import datetime
import os

EMAIL_ACCOUNT = "mabolfadl@gmail.com"
FROM_PWD = "Hamadaguc_2010"
# Use 'INBOX' to read inbox.  Note that whatever folder is specified, 
# after successfully running this script all emails in that folder 
# will be marked as read.
EMAIL_FOLDER = "INBOX"


def save_attachment(msg, download_folder="07_emails"):
        att_path = "No attachment found."
        for part in msg.walk():
            if part.get_content_maintype() == 'multipart':
                continue
            if part.get('Content-Disposition') is None:
                continue

            filename = part.get_filename()
            if filename:
                print("Saving "+filename)
                att_path = os.path.join(download_folder, filename)
    
                if not os.path.isfile(att_path):
                    fp = open(att_path, 'wb')
                    fp.write(part.get_payload(decode=True))
                    fp.close()
        return att_path



M = imaplib.IMAP4_SSL('imap.gmail.com')

try:
    rv, data = M.login(EMAIL_ACCOUNT, FROM_PWD)
except imaplib.IMAP4.error:
    print ("LOGIN FAILED!!! ")
    sys.exit(1)

print(rv, data)

rv, mailboxes = M.list()
if rv == 'OK':
    print("Mailboxes:")
    print(mailboxes)

EMAIL_FOLDER = "INBOX"

rv, data = M.select(EMAIL_FOLDER)

rv, data = M.search(None, "ALL")


if False:
    status, message = M.search(None, 'FROM', "help@xteam.com", 'SUBJECT', "Reset Password")
    status, message = M.search(None, 'SUBJECT', "Uber")
    kw = "Uber"
    kw = kw.encode()
    status, data = M.search(None, 'SUBJECT', kw)
    status, data = M.search(None, 'FROM', "ghadasay@yahoo.com")
    status, data = M.search(None, '(FROM "ghadasay@yahoo.com")')


for num in data[0].split():
        rv, data_curr = M.fetch(num, '(RFC822)')
        if rv != 'OK':
            print("ERROR getting message", num)

        msg = email.message_from_bytes(data_curr[0][1])
        print(msg['Date'])
        save_attachment(msg)
        
        if False:
            hdr = email.header.make_header(email.header.decode_header(msg['Subject']))
            subject = str(hdr)
            print('Message %s: %s' % (num, subject))
            print('Raw Date:', msg['Date'])
            # Now convert to local date-time
            date_tuple = email.utils.parsedate_tz(msg['Date'])
            if date_tuple:
                local_date = datetime.datetime.fromtimestamp(
                    email.utils.mktime_tz(date_tuple))
                print ("Local Date:", \
                    local_date.strftime("%a, %d %b %Y %H:%M:%S"))



M.logout()












def process_mailbox(M):
    """
    Do something with emails messages in the folder.  
    For the sake of this example, print some headers.
    """

    rv, data = M.search(None, "ALL")
    if rv != 'OK':
        print("No messages found!")
        return

    for num in data[0].split():
        rv, data = M.fetch(num, '(RFC822)')
        if rv != 'OK':
            print("ERROR getting message", num)
            return

        msg = email.message_from_bytes(data[0][1])
        hdr = email.header.make_header(email.header.decode_header(msg['Subject']))
        subject = str(hdr)
        print('Message %s: %s' % (num, subject))
        print('Raw Date:', msg['Date'])
        # Now convert to local date-time
        date_tuple = email.utils.parsedate_tz(msg['Date'])
        if date_tuple:
            local_date = datetime.datetime.fromtimestamp(
                email.utils.mktime_tz(date_tuple))
            print ("Local Date:", \
                local_date.strftime("%a, %d %b %Y %H:%M:%S"))


M = imaplib.IMAP4_SSL('imap.gmail.com')

try:
    rv, data = M.login(EMAIL_ACCOUNT, FROM_PWD)
except imaplib.IMAP4.error:
    print ("LOGIN FAILED!!! ")
    sys.exit(1)

print(rv, data)

rv, mailboxes = M.list()
if rv == 'OK':
    print("Mailboxes:")
    print(mailboxes)

EMAIL_FOLDER = "INBOX"

rv, data = M.select(EMAIL_FOLDER)
if rv == 'OK':
    print("Processing mailbox...\n")
    process_mailbox(M)
    M.close()
else:
    print("ERROR: Unable to open mailbox ", rv)

M.logout()