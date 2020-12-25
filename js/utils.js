function updateState(field, value) {
    let newState = { ...window.demoState};
    newState[field] = value;
    console.log(`State updated: [${field}] ${window.demoState[field]} -> ${value}`)
    window.demoState = newState;
}

function stateReactor(fieldName) {
    return function(event) {
        let {value} = event.target;
        updateState(fieldName, value)
    }
}

function generateMatrix(rows, cols) {
    let m = []
    for (let i=0; i<rows; i++){
        m.push([])
        for (let j = 0; j < cols; j++) {
            m[i].push([0])
        }
    }
    return m;
}

function demonstrateState() {
    console.log("Demonstrating")
    document.getElementById("info").innerText = JSON.stringify(window.demoState)
}