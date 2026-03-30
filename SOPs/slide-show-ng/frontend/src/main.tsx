import React from 'react';
import { createRoot } from 'react-dom/client';
import './style.css';
import DeckApp from './DeckApp';

const container = document.getElementById('root');

if (!container) {
  throw new Error('root container not found');
}

createRoot(container).render(
  <React.StrictMode>
    <DeckApp />
  </React.StrictMode>,
);
