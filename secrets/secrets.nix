let key = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDl2xLdn8Q+Iqj89fTONR7uWmVbkXSUGEHyYU2Ax95i9niKk8aWVpaXMiquLe4gcmjohdfh2+X9UM4p99t7xislC0YWqTxr2D82SpJMtbpVc6EPFQbSm10ILRd3R09uL/wS3DkzxzCqrkO0kLdRZLTCnXtvpyRwubRB170rzw3RHfUrqhH/Ou3oIHt8Uw6WLn7QJgGY9xbWYwYQRU5J1ZIsS39PdEmcbYJziv8dPpVo3hF8LgMVzldnyZaEnxFMXBfnkWfAy2cT/jmzXgksilXnFj5w/OIXAm256WFYQK1t/OYjdrp+8wZOivmKyDi4Qfa1/A/R1CMk6DlNqNyS8bHihDKlZ4VQbJbh30NiM7PZQ6QiRvqRmZ2UfIHVn7motedmFYal3/HP4hglAzeeVX4Qdwy/UyLrLdpvnO6Qm/B0RgfwRqs4X20hINxhcCIpQOls2luVrvNeWbY2oHBRc0AE4x/bJahGoTFmuKFcvo0CSAXU7K2s/n1Q8/emLme2+SxYQnommNcH/BZTww/kgAmxIVIj1IXMdbgRbMgri6ym3hmWCUgtn6AGBc5rUZZ1PrukjvXEshhpFILqkEz+FSpI72yRDcvFF9TC0Z4uObdGwcnNfn+GDLwMKQeZQx5GctTKTGbKo1UwdZnEbecdxYIQRj/vByMtMQiZCeGXPn9E4w==";
in {
  "git-config.age".publicKeys = [key];
  }
