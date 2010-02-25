Use tree path to uniquely identify Swing components.
This means that small codebase changes can lead to errors in the future. In fact, I'm not entirely sure that the children list (Container.getComponents()) is ordered.
