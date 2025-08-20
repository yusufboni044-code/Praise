# Praise
 🌐 Google Clarity Web3 Winning Procurement Contract

This project contains a **Clarity smart contract** designed to demonstrate how **Google-scale procurement** could be powered by **Web3 transparency and fairness**.  
It uses the **Stacks blockchain** and Clarinet to manage projects, vendor bids, evaluator scoring, and automatic winner selection.

✨ Key Features

- **Create Procurement Projects**
  - Owners post opportunities with budget, description, and deadlines.
- **Vendor Participation**
  - Vendors submit bids with a cost proposal and off-chain reference (IPFS, Arweave, etc.).
- **Fair Scoring**
  - Evaluators assign quality scores.
  - The contract calculates a **weighted score** based on quality and cost:
    ```
    weighted-score = (quality * 1000) / cost
    ```
- **Automatic Winner Selection**
  - The system selects the best bid using a balance of **low cost** and **high quality**.
- **Full Transparency**
  - Every action is stored on-chain and auditable.

🚀 Quickstart


