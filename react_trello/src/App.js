import React, { Component } from 'react';
import { DragDropContext, Droppable, Draggable } from 'react-beautiful-dnd';
import './App.css';

const __STATE__ = 'state'

const randomRange = (min, max) => Math.round(Math.random() * (max - min) + min)

const randomString = () => Math.random().toString(36).substr(2, 5)

const rawIndexToIndex = (list, rawIndex, keyName, val) => {
  const filtered = list.filter((i) => i[keyName] === val)
  if (!filtered.length || filtered.length === rawIndex) {
    return list.length
  }
  const {belongsTo, id} = filtered[rawIndex]
  return list.reduce((found, item, index) => {
    return item.id === id ? {found: index} :
    (item[keyName] === belongsTo && typeof found === 'number' ? found + 1 :
     found)
  }, 0).found
}

const reorder = (list, rawStartIndex, startParent, rawEndIndex, endParent) => {
  const result = Array.from(list)
  const startIndex = rawIndexToIndex(result, rawStartIndex, 'belongsTo', startParent)
  const endIndex = rawIndexToIndex(result, rawEndIndex, 'belongsTo', endParent)
  const [removed] = result.splice(startIndex, 1)
  result.splice(endIndex, 0, removed)

  return result
}

const getItemStyle = (draggableStyle, isDragging) => ({
  background: isDragging ? 'lightgreen' : 'white',
  ...draggableStyle
})

const getListStyle = isDraggingOver => ({
  background: isDraggingOver ? '#3f51b5' : '#eee'
})

const sampleCards = [
  "webpack",
  "higher order components",
  "redux",
  "mobx",
  "rxjs",
  "compound components",
  "container components",
  "child components",
  "redux sagas",
  "react-router"
].map((t) => ({text: t}))

const sampleColumns = [
  'icebox',
  'backlog',
  'current',
  'completed'
].map((t) => ({title: t, id: randomString()}))

function CardEditor({submit, text}) {
  return (
    <form onSubmit={submit}>
      <textarea autoFocus onBlur={submit} defaultValue={text} />
      <button className='btn save-btn' type='submit' onClick={submit}>Save</button>
    </form>
  )
}

function CardDisplay({text, toggle, showEditBtn}) {
  const editBtn = (
    <div onClick={toggle}>
      <span className='fas fa-pencil-alt btn' />
    </div>
  )
  return (
    <div className='card-display'>
      <div className='card-text noselect'>
        {text}
      </div>
      {showEditBtn ? editBtn : null}
    </div>
  )
}

class Card extends Component {

  state = {showEditBtn: false}
  showEditBtn = () => this.setState({showEditBtn: true})
  hideEditBtn = () => this.setState({showEditBtn: false})

  onSubmit = (e) => {
    this.props.onChangeText(e.target.value)
    this.props.toggle()
    e.preventDefault()
  }

  renderContent() {
    return this.props.editing ?
           (<CardEditor
              submit={this.onSubmit}
              text={this.props.text} />) :
           (<CardDisplay
              toggle={this.props.toggle}
              showEditBtn={this.state.showEditBtn}
              text={this.props.text} />)
  }

  render() {
    return (
      <div key={this.props.id}
        onMouseEnter={this.showEditBtn}
        onMouseLeave={this.hideEditBtn}>
        {this.renderContent()}
      </div>
    )
  }
}

class CardAdd extends Component {

  state = {text: ''}
  changeText = (e) => this.setState({text: e.target.value})
  addCard = () => this.props.onAdd(this.state.text)

  render () {
    const {cancel} = this.props
    return (
      <div>
        <textarea autoFocus onChange={this.changeText} value={this.state.text} className='shadow' />
        <div className='card-edit-row'>
          <div onClick={cancel}>
            <span className='fas fa-times btn cancel-btn' />
          </div>
          <button className='btn save-btn' onClick={this.addCard}>Add</button>
        </div>
      </div>
    )
  }
}

class Column extends Component {
  state = {title: this.props.title, hovering: false}
  changeTitleVal = (e) => this.setState({title: e.target.value})
  changeHovering = (val) => () => this.setState({hovering: val})
  changeTitle = () => this.props.onChangeColumnTitle(this.state.title)
  render() {
    const {
      id,
      children,
      editing,
      toggleCardAdding,
      toggleTitleEditing,
      isAdding,
      onAdd
    } = this.props
    const {title, hovering} = this.state
    const addBtn = (
      <div onClick={toggleCardAdding} className='btn add-text'>Add a card</div>
    )
    const addForm = (
      <CardAdd onAdd={onAdd} cancel={toggleCardAdding} />
    )
    const editTitleForm = (
      <div className='change-col-title'>
        <input onChange={this.changeTitleVal} value={title} />
        <button onClick={this.changeTitle} className='btn save-btn'>Save</button>
      </div>
    )
    const icon = (
      <div>
        <span className='fas fa-pencil-alt btn' />
      </div>
    )
    const titleDisplay = (
      <div className='col-title' onClick={toggleTitleEditing}>
        <div style={{paddingRight: '2em'}}>
          <h4>{title}</h4>
        </div>
        {hovering ? icon : null}
      </div>
    )

    return (
      <div>
        <div onMouseEnter={this.changeHovering(true)}
          onMouseLeave={this.changeHovering(false)}>
          {editing === id ? editTitleForm : titleDisplay}
        </div>
        {children}
        {isAdding ? null : addBtn}
        {isAdding ? addForm : null}
      </div>
    )
  }
}

class App extends Component {
  static generateCards = (numCards) => {
    const createCard = (e) => Object.assign(
      {},
      sampleCards[randomRange(0, 9)],
      {id: randomString(), belongsTo: sampleColumns[randomRange(0, 3)].id}
    )
    return Array.apply(null, Array(numCards))
      .map(createCard)
  }
  static getStateOrGenerate = () => {
    const localState = JSON.parse(window.localStorage.getItem(__STATE__))
    return localState ? localState : {
      currentlyEditing: null,
      currentlyAdding: null,
      columns: sampleColumns,
      cards: App.generateCards(10)
    }
  }

  state = App.getStateOrGenerate()
  hydrateLocal = () => window.localStorage.setItem(__STATE__, JSON.stringify(this.state))

  onDragEnd = (result) => {
    if (!result.destination) {
      return;
    }
    const changeParent = (childId, parentId) => (c) => c.id === childId && c.belongsTo !== parentId ? Object.assign({}, c, {belongsTo: parentId}) : c

    const cards = reorder(
      this.state.cards,
      result.source.index,
      result.source.droppableId,
      result.destination.index,
      result.destination.droppableId,
    ).map(
      changeParent(
        result.draggableId,
        result.destination.droppableId)
    );

    this.setState({cards}, this.hydrateLocal);
  }

  toggleCardAdding(id) {
    const {currentlyAdding} = this.state
    return () => this.setState({
      currentlyAdding: currentlyAdding === id ? null : id
    }, this.hydrateLocal)
  }

  toggleEditing(id) {
    const {currentlyEditing} = this.state
    return (cardIndex) => this.setState({
      currentlyEditing: currentlyEditing === id ? null : id
    }, this.hydrateLocal)
  }

  changeCardText(id) {
    const {cards} = this.state
    const changeCard = (change) => (c) => c.id === id ? Object.assign({}, c, change) : c

    return (text) => this.setState({
      cards: cards.map(changeCard({text: text}))
    }, this.hydrateLocal)
  }

  addCard(id) {
    return (text) => {
      this.setState({
        cards: this.state.cards.concat({id: randomString(), text: text, belongsTo: id}),
        currentlyAdding: null
      }, this.hydrateLocal)
    }
  }

  changeColTitle(colId) {
    const changeColumn = (change) => (c) => c.id === colId ? Object.assign({}, c, change) : c
    return (text) => {
      this.setState({
        columns: this.state.columns.map(changeColumn({title: text})),
        currentlyEditing: null
      }, this.hydrateLocal)
    }
  }

  renderCards(colId) {
    const {currentlyEditing} = this.state
    const renderCard = (c, i) => {
      const card = (provided, snapshot) => (
        <div>
          <div className='card'
            style={getItemStyle(
              provided.draggableStyle,
              snapshot.isDragging
            )}
            {...provided.dragHandleProps}
            ref={provided.innerRef}>
            <Card
              text={c.text}
              editing={c.id === currentlyEditing}
              toggle={this.toggleEditing(c.id)}
              onChangeText={this.changeCardText(c.id)} />
          </div>
          {provided.placeholder}
        </div>
      )

      return (
        <Draggable key={c.id} draggableId={c.id}>
          {card}
        </Draggable>
      )
    }
    return this.state.cards
      .filter((c) => c.belongsTo === colId)
      .map(renderCard)
  }

  renderColumns() {
    const {currentlyEditing, currentlyAdding} = this.state
    const renderColumn = (c, i) => {
      const column = (provided, snapshot) => (
        <div ref={provided.innerRef} className="column"
          style={getListStyle(snapshot.isDraggingOver)}>
          <Column key={i}
            id={c.id}
            title={c.title}
            cards={c.cards}
            onChangeColumnTitle={this.changeColTitle(c.id)}
            toggleTitleEditing={this.toggleEditing(c.id)}
            editing={currentlyEditing}
            toggleCardAdding={this.toggleCardAdding(c.id)}
            onAdd={this.addCard(c.id)}
            isAdding={currentlyAdding === c.id}
            onChangeCardText={this.changeCardText}>
            {this.renderCards(c.id)}
            {provided.placeholder}
          </Column>
        </div>
      )
      return (
        <Droppable key={c.id} droppableId={c.id}>
          {column}
        </Droppable>
      )
    }
    return this.state.columns.map(renderColumn)
  }

  render() {
    return (
      <div className="App">
        <header className="App-header">
          <h1 className="App-title">Trello in React</h1>
        </header>
        <p><i>Changes are saved locally</i></p>
        <div className="row-container">
          <DragDropContext onDragEnd={this.onDragEnd}>
            {this.renderColumns()}
          </DragDropContext>
        </div>
      </div>
    );
  }
}

export default App;
