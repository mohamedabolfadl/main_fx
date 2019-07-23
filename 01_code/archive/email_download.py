# -*- coding: utf-8 -*-
"""
Created on Tue Apr 16 17:47:21 2019

@author: Mohamed Ibrahim
"""



import smtplib
import time
import imaplib
import email
import os
import re

ORG_EMAIL   = "@gmail.com"
FROM_EMAIL  = "mabolfadl" + ORG_EMAIL
FROM_PWD    = "Hamadaguc_2010"
SMTP_SERVER = "imap.gmail.com"
SMTP_PORT   = 993



ml = FetchEmail(SMTP_SERVER ,FROM_EMAIL, FROM_PWD)
unread = ml.fetch_unread_messages()


class FetchEmail():

    connection = None
    error = None

    def __init__(self, mail_server, username, password):
        self.connection = imaplib.IMAP4_SSL(mail_server)
        self.connection.login(username, password)
        self.connection.select(readonly=False) # so we can mark mails as read

    def close_connection(self):
        """
        Close the connection to the IMAP server
        """
        self.connection.close()

    def save_attachment(self, msg, download_folder="07_emails"):
        """
        Given a message, save its attachments to the specified
        download folder (default is /tmp)

        return: file path to attachment
        """
        att_path = "No attachment found."
        for part in msg.walk():
            if part.get_content_maintype() == 'multipart':
                continue
            if part.get('Content-Disposition') is None:
                continue

            filename = part.get_filename()
            att_path = os.path.join(download_folder, filename)

            if not os.path.isfile(att_path):
                fp = open(att_path, 'wb')
                fp.write(part.get_payload(decode=True))
                fp.close()
        return att_path

    def fetch_unread_messages(self):
        """
        Retrieve unread messages
        """
        emails = []
        (result, messages) = self.connection.search(None, 'UnSeen')
        if result == "OK":
            for message in messages[0].split(' '):
                try: 
                    ret, data = self.connection.fetch(message,'(RFC822)')
                except:
                    self.close_connection()
                    exit()

                msg = email.message_from_bytes(data[0][1])
                if isinstance(msg, str) == False:
                    emails.append(msg)
                response, data = self.connection.store(message, '+FLAGS','\\Seen')

            return emails

        self.error = "Failed to retreive emails."
        return emails

    def parse_email_address(self, email_address):
        """
        Helper function to parse out the email address from the message

        return: tuple (name, address). Eg. ('John Doe', 'jdoe@example.com')
        """
        return email.utils.parseaddr(email_address)

        

# -------------------------------------------------
#
# Utility to read email from Gmail Using Python
#
# ------------------------------------------------

mail = imaplib.IMAP4_SSL(SMTP_SERVER)
mail.login(FROM_EMAIL,FROM_PWD)


mail.select('inbox')

type, data = mail.search(None, 'ALL')
mail_ids = data[0]
id_list = mail_ids.split()


for num in data[0].split():
    typ, data = mail.fetch(num, '(RFC822)' )
    raw_email = data[0][1]
    #re.search()
# converts byte literal to string removing b''
#    raw_email_string = raw_email.decode('utf-8')
    raw_email_string = "".join(map(chr, raw_email) )

    
    email_message = email.message_from_string(raw_email_string)
    
    
    
# downloading attachments
    for part in email_message.walk():
        # this part comes from the snipped I don't understand yet... 
        if part.get_content_maintype() == 'multipart':
            continue
        if part.get('Content-Disposition') is None:
            continue
        fileName = part.get_filename()
        if bool(fileName):
            filePath = os.path.join('/Users/sanketdoshi/python/', fileName)
            if not os.path.isfile(filePath) :
                fp = open(filePath, 'wb')
                fp.write(part.get_payload(decode=True))
                fp.close()
            subject = str(email_message).split("Subject: ", 1)[1].split("\nTo:", 1)[0]
            print('Downloaded "{file}" from email titled "{subject}" with UID {uid}.'.format(file=fileName, subject=subject, uid=latest_email_uid.decode('utf-8')))




mail = imaplib.IMAP4_SSL('imap.gmail.com')
mail.login(FROM_EMAIL, FROM_PWD)
mail.select('inbox')

#get uids of all messages
result, data = mail.uid('search', None, 'ALL') 
uids = data[0].split()

#read the lastest message
result, data = mail.uid('fetch', uids[-1], '(RFC822)')

m = email.message_from_string(data[0][1])
m = email.message_from_string(data[0])

if m.get_content_maintype() == 'multipart': #multipart messages only
    for part in m.walk():
        #find the attachment part
        if part.get_content_maintype() == 'multipart': continue
        if part.get('Content-Disposition') is None: continue

        #save the attachment in the program directory
        filename = part.get_filename()
        fp = open(filename, 'wb')
        fp.write(part.get_payload(decode=True))
        fp.close()
        print '%s saved!' % filename
        
        

def read_email_from_gmail():
#    try:
        mail = imaplib.IMAP4_SSL(SMTP_SERVER)
        mail.login(FROM_EMAIL,FROM_PWD)
        mail.select('inbox')


        type, data = mail.search(None, 'ALL')
        mail_ids = data[0]
        print(mail_ids)
        id_list = mail_ids.split()   
        first_email_id = int(id_list[0])
        latest_email_id = int(id_list[-1])
        

        for i in range(latest_email_id,first_email_id, -1):
            typ, data = mail.fetch(i, '(RFC822)' )

            for response_part in data:
                if isinstance(response_part, tuple):
                    msg = email.message_from_string(response_part[1])
                    email_subject = msg['subject']
                    email_from = msg['from']
                    print('From : ' + email_from + '\n')
                    print('(Subject : ' + email_subject + '\n')

#    except Exception, e:
        #print(str(e))
