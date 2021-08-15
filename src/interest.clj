;;; It is common in business development lending for a Borrrower to have to take a loan out for
;;; both the capital and the interest due on this capital. 
;;; This is typically referred to as RolledUp interest, 
;;; as opposed to Serviced interest - where borrower pays the interest themselves from a separate revenue stream.
;;; See https://www.propertyfinancegroup.com/glossary/rolled-up-interest/
(ns interest)

(defn rolledUp-interest
  "Given an amount of capital required and an interest rate (for the entire loan).
  Calculate the GrossLoan amount needed that includes capital+interest.
  NOTE: This function is easy to understand but the single line version rolledUp-interest-single-equation
  is the one to use."
  [amount interest-rate]
  (let [intAmount (* amount interest-rate)]
    ;;(prn "intAmount" intAmount)
    (if (< intAmount 0.01)
      amount
      (+ amount (rolledUp-interest intAmount interest-rate)))))

;; Here's a single line equation for working it out
(defn rolledUp-interest-single-equation
  [amount interest-rate]
  (/ amount (- 1 interest-rate)))


(comment
;; Call the rolledUp-interest function for a borrower that needs £1000 at interest rate 10%
(rolledUp-interest 1000 0.1)
(rolledUp-interest-single-equation 1000 0.1)

;; Both of the methods for calculating only work for interest-rate < 1
;; rolledUp-interest will blow the stack if you have an interest-rate >0.99
;; rolledUp-interest-single-equation works upto 1 and then you get a divide-by-zero
;; What both are telling us is that if the interest-rate is 100% or > of the amount then
;; it is impossible to lend the interest because the interest on this
;; interest will be greater than the original amount, and you then need to borrow to pay the interest
;; on the interest, ...
(rolledUp-interest 1000 0.99) ;; This is the biggest interest value that will work before it crashes
(rolledUp-interest-single-equation 1000 1) ;; This will crash. Reduce to below 1 and see the large GrossAmount required!!!

;; Do a range of tests
(for [amount [1000 30000099] interest [0.1 0.2 0.3 0.5 0.5 0.6 0.75 0.81 0.9999]]
  (let [ga1 (rolledUp-interest amount interest)
        ga2 (rolledUp-interest-single-equation amount interest)]

    (prn "Amount:" amount "iRate:" interest "GA1:" ga1 "GA1:" ga2)))

;; Working out the number of secs into a video recording :)
(+ 20 (* 27 60))
)