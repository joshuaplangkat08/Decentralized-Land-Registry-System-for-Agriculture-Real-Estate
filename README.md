# 🏗️ Decentralized Land Registry System

A blockchain-based land registry system built on Stacks, enabling transparent and tamper-proof land ownership management.

## 🎯 Features

- ✨ Register new land parcels with coordinates and details
- 🔄 Transfer land ownership between parties
- 📊 Track complete land ownership history
- 🔍 Verify land ownership status
- 🔐 Administrative controls for land status updates
- 📍 Coordinate system for precise boundary definition

## 🚀 Contract Functions

### Administrative Functions
- `register-land`: Register new land parcels (admin only)
- `change-admin`: Transfer admin rights
- `update-land-status`: Update land status (admin only)
- `update-coordinates`: Update land coordinates (admin only)

### Public Functions
- `transfer-land`: Transfer land ownership
- `verify-ownership`: Verify if an address owns specific land
- `get-land-details`: Retrieve complete land details
- `get-land-history`: View ownership transfer history
- `get-owner`: Get current land owner
- `get-transaction-count`: Get total number of transactions

## 💻 Usage Example

```clarity
;; Register new land
(contract-call? .land-registry register-land u1 (list u1 u2 u3 u4 u5 u6 u7 u8) u1000 "agricultural")

;; Transfer land
(contract-call? .land-registry transfer-land u1 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)

;; Check land details
(contract-call? .land-registry get-land-details u1)
```

## 🔒 Security

- Only registered admin can perform administrative functions
- Ownership verification required for transfers
- Immutable transaction history
- Error handling for all operations

## 🌟 Future Enhancements

- Multi-signature requirements for transfers
- Land subdivision functionality
- Integration with mapping services
- Document attachment capability
```

