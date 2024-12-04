# Geração de número aleatórios uniformes sobre uma superficie em 3D

# Raydonal & CHarlize Theron Godel PTolemy

import numpy as np
import plotly.graph_objects as go

#=======================================================
# Função para gerar pontos uniformemente em um triângulo

def sample_triangle(v1, v2, v3, num_points):
    """
    Gera pontos uniformemente distribuídos sobre um triângulo definido pelos vértices v1, v2, v3.
    
    Parâmetros:
        v1, v2, v3: Coordenadas dos vértices do triângulo.
        num_points: Número de pontos a serem gerados.

    Retorna:
        Um array de pontos com coordenadas (x, y, z).
    """
    u = np.sqrt(np.random.rand(num_points))  # Correção para uniformidade
    v = np.random.rand(num_points)
    mask = u + v > 1
    u[mask] = 1 - u[mask]
    v[mask] = 1 - v[mask]
    w = 1 - u - v
    points = u[:, None] * v1 + v[:, None] * v2 + w[:, None] * v3
    return points

# Função para visualização dos pontos no triângulo
def visualize_triangle_with_points(v1, v2, v3, num_points):
    """
    Visualiza o triângulo e os pontos gerados sobre sua superfície.
    
    Parâmetros:
        v1, v2, v3: Coordenadas dos vértices do triângulo.
        num_points: Número de pontos a serem gerados.
    """
    points = sample_triangle(v1, v2, v3, num_points)

    # Criar uma figura 3D
    fig = go.Figure()

    # Adicionar o triângulo como uma superfície
    fig.add_trace(go.Mesh3d(
        x=[v1[0], v2[0], v3[0]],
        y=[v1[1], v2[1], v3[1]],
        z=[v1[2], v2[2], v3[2]],
        # i=[0],
        # j=[1],
        # k=[2],
      color = "lightgray",
        opacity=0.5,
        name='Triângulo',
showlegend=False  # Remover da legenda
    ))
    
        # Adicionar arestas escuras
    edges = [(v1, v2), (v2, v3), (v3, v1)]
    for edge in edges:
        fig.add_trace(go.Scatter3d(
            x=[edge[0][0], edge[1][0]],  # Coordenadas X da aresta
            y=[edge[0][1], edge[1][1]],  # Coordenadas Y da aresta
            z=[edge[0][2], edge[1][2]],  # Coordenadas Z da aresta
            mode='lines',
            line=dict(color='darkblue', width=5),  # Arestas mais escuras
            name='Aresta',
            showlegend=False  # Remover da legenda
        ))

    # Adicionar os pontos sobre o triângulo
    fig.add_trace(go.Scatter3d(
        x=points[:, 0],
        y=points[:, 1],
        z=points[:, 2],
        mode='markers',
        marker=dict(size=3, color='darkred'),
        name='Pontos Uniformemente distribuídos',
                    showlegend=False  # Remover da legenda
    ))

    # Configurar o layout
    fig.update_layout(
        scene=dict(
            xaxis=dict(title='X', backgroundcolor="white", gridcolor="lightgray"),
            yaxis=dict(title='Y', backgroundcolor="white", gridcolor="lightgray"),
            zaxis=dict(title='Z', backgroundcolor="white", gridcolor="lightgray"),
            aspectmode='data'
        ),
        plot_bgcolor="white",  # Background do gráfico
        paper_bgcolor="white",  # Background geral
        # title='Pontos distribuidos uniformemente no Triângulo',
        # showlegend=False  # Remover da legenda
    )
    
    # Mostrar a visualização
    fig.show()

# Definir os vértices do triângulo
v1 = np.array([0.5, 0, 0])
v2 = np.array([0, 0, 0.5])
v3 = np.array([0.3, 0.7, 0.3])  # Triângulo equilátero no plano XY

# Número de pontos a serem gerados
num_points = 500

# Visualizar
visualize_triangle_with_points(v1, v2, v3, num_points)



#=======================================================
# Função para gerar pontos uniformemente em um quadrilátero

import numpy as np
import plotly.graph_objects as go

# Função para gerar pontos uniformemente sobre um quadrilátero
def sample_quadrilateral(v1, v2, v3, v4, num_points):
    """
    Gera pontos uniformemente distribuídos sobre um quadrilátero definido pelos vértices v1, v2, v3 e v4.
    
    Parâmetros:
        v1, v2, v3, v4: Coordenadas dos vértices do quadrilátero (em sentido horário ou anti-horário).
        num_points: Número de pontos a serem gerados.

    Retorna:
        Um array de pontos com coordenadas (x, y, z).
    """
    u = np.random.rand(num_points)  # Coordenada U no intervalo [0, 1]
    v = np.random.rand(num_points)  # Coordenada V no intervalo [0, 1]

    # Fórmula bilinear para interpolação dentro do quadrilátero
    points = ((1 - u) * (1 - v))[:, None] * v1 + \
             (u * (1 - v))[:, None] * v2 + \
             (u * v)[:, None] * v3 + \
             ((1 - u) * v)[:, None] * v4
    return points

# Função para visualização do quadrilátero e dos pontos
def visualize_quadrilateral_with_points(v1, v2, v3, v4, num_points):
    """
    Visualiza o quadrilátero e os pontos gerados sobre sua superfície.
    
    Parâmetros:
        v1, v2, v3, v4: Coordenadas dos vértices do quadrilátero.
        num_points: Número de pontos a serem gerados.
    """
    points = sample_quadrilateral(v1, v2, v3, v4, num_points)

    # Criar uma figura 3D
    fig = go.Figure()

    # Adicionar o quadrilátero como uma superfície
    fig.add_trace(go.Mesh3d(
        x=[v1[0], v2[0], v3[0], v4[0]],
        y=[v1[1], v2[1], v3[1], v4[1]],
        z=[v1[2], v2[2], v3[2], v4[2]],
        # i=[0, 0],
        # j=[1, 3],
        # k=[2, 2],
        # l=[3, 3],
      color = "lightgray",
        opacity=0.5,
        name='Quadrilátero',
      showlegend=False  # Remover da legenda
    ))



# Adicionar arestas escuras
    edges = [(v1, v2), (v2, v3), (v3, v4), (v4, v1)]  # Definir as arestas
    for edge in edges:
        fig.add_trace(go.Scatter3d(
            x=[edge[0][0], edge[1][0]],
            y=[edge[0][1], edge[1][1]],
            z=[edge[0][2], edge[1][2]],
            mode='lines',
         line=dict(color='darkblue', width=5),  # Arestas mais escuras
               name='Aresta',
        showlegend=False  # Remover da legenda
        ))

    # Adicionar

    # Adicionar os pontos sobre o quadrilátero
    fig.add_trace(go.Scatter3d(
        x=points[:, 0],
        y=points[:, 1],
        z=points[:, 2],
        mode='markers',
        marker=dict(size=2, color='red'),
        name='Pontos Uniformes',
        showlegend=False  # Remover da legenda
    ))

    # Configurar o layout
    fig.update_layout(
        scene=dict(
            xaxis=dict(title='X', backgroundcolor="white", gridcolor="lightgray"),
            yaxis=dict(title='Y', backgroundcolor="white", gridcolor="lightgray"),
            zaxis=dict(title='Z', backgroundcolor="white", gridcolor="lightgray"),
            aspectmode='data'
        ),
        plot_bgcolor="white",  # Background do gráfico
        paper_bgcolor="white",  # Background geral
        # title='Pontos distribuidos uniformemente no Triângulo',
        # showlegend=False  # Remover da legenda
    )
    
    # Mostrar a visualização
    fig.show()

# Definir os vértices do quadrilátero
v1 = np.array([0, 0, 0])
v2 = np.array([1, 0, 0])
v3 = np.array([1, 1, 0])
v4 = np.array([0, 1, 0])
# Número de pontos a serem gerados
num_points = 500

# Visualizar
visualize_quadrilateral_with_points(v1, v2, v3, v4, num_points)

