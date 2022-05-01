var ones = ["", "uno", "du", "tri", "tetr", "pent", "sex", "hept", "oct", "non"]
var tens = ["", "dec", "vigint", "trigint", "quadrint", "quinquagint", "sexagint", "septuagint", "octogint", "nonagint"]

var inp = document.querySelector("input")
var out = document.querySelector("#nameout")

inp.addEventListener("keyup", () => {
    out.disabled = false
    let num = inp.value
    let one = Number(String(num).charAt(1))
    let ten = Number(String(num).charAt(0))
    if (num.toString().length == 1) {
        if (num == 1) {
            out.innerText = "Play the daily Wordle"
        } else if (num == 0 || num == "-" || num == "." || num == "e") {
            out.innerText = "Play"
            out.disabled = true
        } else {
            out.innerText = "Play the daily " + ones[num].charAt(0).toUpperCase() + ones[num].slice(1) + "ordle"
        }
    } else if (num.toString().length == 2 & !num.includes("-") & !num.includes(".") & !num.includes("e")) {
        let wordle = ones[one] + (one > 3 ? "a" : "") + tens[ten] + "ordle"
        wordle = wordle.charAt(0).toUpperCase() + wordle.slice(1)
        out.innerText = "Play the daily " + wordle
    } else {
        out.innerText = "Play"
        out.disabled = true
    }
})

out.addEventListener("click", () => {
    let g = Number(inp.value)
    document.querySelector("#subtitle").classList.add("anim")
    document.querySelector("#start").classList.add("anim")
    document.querySelector("#gamediv").innerHTML = ("<table class='game'>" + ("<tr>" + "<td class='open'>e</td>".repeat(5) + "</tr>") + ("<tr>" + "<td class='empty'></td>".repeat(5) + "</tr>").repeat(g + 5) + "</table>").repeat(g)
    document.querySelector("#gamediv").classList.add("anim")
})

document.querySelector("#start").addEventListener("transitionend", () => {
    document.querySelector("#start").remove()
    document.querySelector("#subtitle").innerText = out.innerText.split(" ")[3]
})
