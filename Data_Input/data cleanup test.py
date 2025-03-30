import csv
import re

#Making names for the input file (RawData.csv) and output file (data_cleaned.csv)

input_file = 'RawData.csv'
output_file = 'data_cleaned.csv'

#Reading the CSV input file
with open(input_file, newline='', encoding='utf-8') as csvfile:
    reader = csv.DictReader(csvfile)
    data = [row for row in reader]

#Converting all the columns to numeric, except for the columns Sample.Name, Batch.Numer, Class.name, Sex, and Age.
#Converting any exceptions to NA
for row in data:
    for column in row:
        if column not in ['Sample.Name', 'Batch.Number', 'Class.name', 'Sex', 'Age']:
            try:
                row[column] = float(row[column])
            except ValueError:
                row[column] = "NA"

#Taking all the numeric columns except for CT and replacing NAs with 0, as all non-numeric values in these columns represented 0 (eg. No Peak)
#For CT, the NA values were not changed, as these represent samples which did not have positive responses to PCR, and so NA makes sense
for row in data:
    for i in range(3, 144):
        column = list(row.keys())[i]
        if row[column] == "NA":
            row[column] = 0

#Checking if values in Age are integers, changing these to int, and then changing all other values to NA
#Age column had multiple values that did not make sense as ages (eg. 0.416666667) so all non-integer values were converted to NA.
for row in data:
    try:
        if re.match(r"^\d+$", row['Age']):
            row['Age'] = int(row['Age'])
        else:
            row['Age'] = "NA"
    except KeyError:
        row['Age'] = "NA"

# Write the cleaned data to a new CSV file
with open(output_file, mode='w', newline='', encoding='utf-8') as csvfile:
    fieldnames = data[0].keys()
    writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
    writer.writeheader()
    writer.writerows(data)

print(f"RawData.csv was transformed and the cleaned dataset is now called {output_file}")
