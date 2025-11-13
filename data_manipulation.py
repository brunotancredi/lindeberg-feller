import bigjson
import pandas as pd

data = {
    'id': [],
    'cites': []
}

max_items = 100  # limit
count = 0

with open('dblp.v12.json', 'rb') as f:
    j = bigjson.load(f)
    for record in j:
        data['id'].append(record.get("id", None))
        data['cites'].append(record.get("n_citation", None))
        count += 1

        print("RECORD: " , count)
        # if count >= max_items:
        #     break

df = pd.DataFrame(data)

#Save dataframe
df.to_csv('dblp_citations.csv', index=False)
