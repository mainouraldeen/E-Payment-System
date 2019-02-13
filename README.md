# E-Payment-System
An e-payment system is a way of making transactions or paying for goods and services through an electronic 
medium, without the use of checks or cash. It’s also called an electronic payment system or online payment 
system. We will be focusing on E-payment by credit card.
Requirements:
You will write an assembly program that takes credit card info and payment amount and checks if credit card 
number and its info are valid and if the payment amount is available in balance it goes through with the 
transaction.
Input/Output:
Prompt the user with the following choices:
1. Add new credit card to the database:
a. Input:
i. Credit Card Number (number must have between 13 and 16 digits. It must start with: 4 
for Visa cards and 5 for Master cards Ex: 4388576018402626)
ii. Expiry date (in the form MM/YY EX: 10/18)
iii. CVV (3 digit number)
iv. Card Holder Name
v. Balance
b. Output:
i. If credit card number is valid and name is not already in database, print the message 
“Credit card successfully added”.
ii. If credit card number is not valid or name is not already in database, print the message 
“Adding Credit card Failed” mentioning the failure reason.
2. Delete credit card from database:
a. Input:
i. User chooses to search by credit card number or card holder name.
b. Output:
i. If credit card found, print the message “Credit Card Deleted Successfully”.
ii. If credit card not found, print the message “Credit Card not fount”.
3. Top-up credit card balance in the database:
a. Input:
i. User chooses to search by credit card number or card holder name.
ii. Top-up amount.
b. Output:
i. If credit card found, print the message “Credit Card balance Toped-up successfully”.
ii. If credit card not found, print the message “Credit Card not fount”.
4- Pay by credit card:
 Input:
iii. Credit Card Number (number must have between 13 and 16 digits. It must start with: 4 
for Visa cards and 5 for Master cards Ex: 4388576018402626)
iv. Expiry date (in the form MM/YY EX: 10/18)
v. CVV (3 digit number)
vi. Card Holder Name
vii. Payment Amount
c. Output:
i. If the transaction is valid, print the message “Your transaction is complete. Your 
payment has been successfully processed”.
ii. If the transaction is not valid, print the message “Your transaction failed. No amount 
was debit from your balance” and mention the reason why it failed.
