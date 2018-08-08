# udh_parser : parser of User Data Header for concatenated SMSs

## [Description](https://en.wikipedia.org/wiki/Concatenated_SMS#Sending_a_concatenated_SMS_using_a_User_Data_Header)
One way of sending concatenated SMS (CSMS) is to split the message into 153 7-bit character parts (134 octets), and sending each part with a User Data Header (UDH) tacked onto the beginning. A UDH can be used for various purposes and its contents and size varies accordingly, but a UDH for concatenating SMSes look like this:

* Field 1 (1 octet): Length of User Data Header, in this case 05.
* Field 2 (1 octet): Information Element Identifier, equal to 00 (Concatenated short messages, 8-bit reference number)
* Field 3 (1 octet): Length of the header, excluding the first two fields; equal to 03
* Field 4 (1 octet): 00-FF, CSMS reference number, must be same for all the SMS parts in the CSMS
* Field 5 (1 octet): 00-FF, total number of parts. The value shall remain constant for every short message which makes up the concatenated short message. If the value is zero then the receiving entity shall ignore the whole information element
* Field 6 (1 octet): 00-FF, this part's number in the sequence. The value shall start at 1 and increment for every short message which makes up the concatenated short message. If the value is zero or greater than the value in Field 5 then the receiving entity shall ignore the whole information element. [ETSI Specification: GSM 03.40 Version 5.3.0: July 1996]

It is possible to use a 16 bit CSMS reference number in order to reduce the probability that two different concatenated messages are sent with identical reference numbers to a receiver. In this case, the User Data Header shall be:

* Field 1 (1 octet): Length of User Data Header (UDL), in this case 06.
* Field 2 (1 octet): Information Element Identifier, equal to 08 (Concatenated short messages, 16-bit reference number)
* Field 3 (1 octet): Length of the header, excluding the first two fields; equal to 04
* Field 4 (2 octets): 0000-FFFF, CSMS reference number, must be same for all the SMS parts in the CSMS
* Field 5 (1 octet): 00-FF, total number of parts. The value shall remain constant for every short message which makes up the concatenated short message. If the value is zero then the receiving entity shall ignore the whole information element
* Field 6 (1 octet): 00-FF, this part's number in the sequence. The value shall start at 1 and increment for every short message which makes up the concatenated short message. If the value is zero or greater than the value in Field 5 then the receiving entity shall ignore the whole information element. [ETSI Specification: GSM 03.40 Version 5.3.0: July 1996]

Example of the UDH for an sms split into two parts:
```sh
05 00 03 CC 02 01 [ message ] 
05 00 03 CC 02 02 [ message ]
```
Note if a UDH is present and the data encoding is the default 7-bit alphabet, the user data must be 7-bit word aligned after the UDH.[3] This means up to 6 bits of zeros need to be inserted at the start of the [message].

E.g. with a UDH containing a single part,

`05 00 03 CC 01 01`

the UDH is a total of (number of octets x bit size of octets) 6 x 8 = 48 bits long. Therefore, a single bit of padding has to be prepended to the message. The UDH is therefore (bits for UDH / bits per septet) = (48 + 1)/7 = 7 septets in length.

With a message of "Hello world", the [message] is encoded as

 `90 65 36 FB 0D BA BF E5 6C 32`
 
as you need to prepend the least significant bits of the next 7bit character whereas without padding, the [message] would be

 `C8 32 9B FD 06 DD DF 72 36 19`

and the UDL is 7 (header septets) + 11 (message septets) = 18 septets.
